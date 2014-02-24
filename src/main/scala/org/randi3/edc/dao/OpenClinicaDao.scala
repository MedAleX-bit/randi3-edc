package org.randi3.edc.dao

import org.randi3.utility.{ SecurityComponent, UtilityDBComponent, Logging }
import scalaz.{ Success, Failure, Validation }
import org.randi3.edc.model.openClinica._
import org.randi3.dao.{ CriterionDaoComponent, TrialDaoComponent, DaoComponent }
import scala.slick.session.Database._
import scala.Left
import scalaz.Failure
import scala.Right
import scalaz.Success
import org.randi3.edc.model.openClinica.FormOC
import org.randi3.edc.model.openClinica.TrialOC
import org.randi3.edc.model.openClinica.ItemGroupOC
import org.randi3.edc.model.openClinica.EventOC
import org.randi3.model.criterion.{ OrdinalCriterion, Criterion }
import org.randi3.model.criterion.constraint.Constraint
import collection.mutable.ListBuffer
import org.randi3.edc.schema.OpenClinicaDatabaseSchema
import org.randi3.model.{ TrialStatus, Trial }
import org.randi3.service.TrialServiceComponent
import scala.slick.lifted.{ Query, Parameters }
import scala.collection._
import scala.collection.immutable.Map

trait OpenClinicaDaoComponent {

  this: DaoComponent with Logging with UtilityDBComponent with TrialDaoComponent with TrialServiceComponent with CriterionDaoComponent with SecurityComponent =>

  val openClinicaSchema: OpenClinicaDatabaseSchema
  val openClinicaDao: OpenClinicaDao

  class OpenClinicaDao {

    import driver.Implicit._
    import schema._
    import openClinicaSchema._
    import utilityDB._
    import securityUtility._

    private def allTrials = database withSession {
      Query(TrialOCs).list
    }

    private val queryTrialOCFromOid = for {
      oid <- Parameters[String]
      trial <- TrialOCs if trial.oid is oid
    } yield trial

    private val queryTrialOCFromTrialSiteOid = for {
      oid <- Parameters[String]
      trialSiteMapping <- TrialSiteMapping if trialSiteMapping.value is oid
    } yield trialSiteMapping.trialId

    private val queryTrialOCFromId = for {
      id <- Parameters[Int]
      trial <- TrialOCs if trial.id is id
    } yield trial

    private val queryConnectionOCFromTrialOCId = for {
      trialOCid <- Parameters[Int]
      connection <- Connections if connection.trialOCId is trialOCid
    } yield connection

    private def queryTrialSiteMapping(trialOCid: Int) = for {
      trialSites <- TrialSiteMapping if trialSites.trialId === trialOCid
    } yield trialSites

    private def queryEventOCFromOidAndTrialOCid(oid: String, trialOCid: Int) = for {
      event <- EventOCs if event.trialOCId === trialOCid && event.oid === oid
    } yield event

    private def queryFormOCFromOidAndEventOCid(oid: String, eventOCid: Int) = for {
      form <- FormOCs if form.eventOCId === eventOCid && form.oid === oid
    } yield form

    private def queryItemGroupOCFromOidAndFormOCid(oid: String, formOCid: Int) = for {
      itemGroup <- ItemGroupOCs if itemGroup.formOCId === formOCid && itemGroup.oid === oid
    } yield itemGroup

    private def queryItemOCFromOidAndItemGroupOCid(oid: String, itemGroupOCid: Int) = for {
      item <- ItemOCs if item.itemGroupOCId === itemGroupOCid && item.oid === oid
    } yield item

    private def queryItemOIDfromId(id: Int) = for {
      item <- ItemOCs if item.id === id
    } yield item

    private def queryOrdinalValueMappingFromItemOC(itemOCId: Int) = for {
      ordinalValueMapping <- OrdinalValueMapping if ordinalValueMapping.itemOCId === itemOCId
    } yield ordinalValueMapping

    def create(trialOC: TrialOC): Validation[String, TrialOC] = {
      onDB {
        val trial = {
          for {
            trialId <- trialDao.create(trialOC.trial.getOrElse(return Failure("Trial must be set")).copy(isEDCTrial = true, status = TrialStatus.ACTIVE))
            trialOption <- trialDao.get(trialId)
          } yield trialOption.getOrElse(return Failure("Trial couldn't be stored."))
        } match {
          case Success(trial) => trial
          case Failure(failure) => return Failure(failure)
        }

        for {
          rowsChangedForCreate <- onDBWithTransaction { Success(TrialOCs.noId insert (trialOC.identifier, trialOC.oid, trialOC.name, trialOC.metaDataVersionOID, trial.id)) }
          trialOCid <- getId(trialOC.oid)
          itemsRes <- createItems(trialOCid, trialOC, trial)
          connectionRes <- createConnection(trialOCid, trialOC)
          trialSiteMappingRes <- createTrialSiteMapping(trialOCid, trialOC)
          treatmentRes <- createTreatmentItem(trialOCid, trialOC, trial)
          trialOCDB <- get(trialOCid)
        } yield trialOCDB
      }
    }

    private def createTrialSiteMapping(trialOCid: Int, trialOC: TrialOC): Validation[String, Boolean] = {
      onDBWithTransaction {
        TrialSiteMapping.noId insertAll (trialOC.sites.map(site => (trialOCid, site._1, site._2)).toSeq: _*)
        Success(true)
      }
    }

    private def createConnection(trialOCid: Int, trialOC: TrialOC): Validation[String, Boolean] = {
      onDBWithTransaction {
        Connections.noId insert (trialOCid, trialOC.connection.location, trialOC.connection.username, trialOC.connection.passwordHash, trialOC.connection.dataSetId)
        Success(true)
      }
    }

    private def createItems(trialOCid: Int, trialOC: TrialOC, trial: Trial): Validation[String, Boolean] = {
      trial.criterions.foreach(criterion => {
        trialOC.getMappedElementsFromCriteria(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]) match {
          case None => return Failure("Criterion in OpenClinica trial not found: " + criterion.name)
          case Some(fieldSet) => {
            createFieldSet(trialOCid, criterion, fieldSet).toEither match {
              case Left(failure) => return Failure(failure)
              case Right(x) =>
            }
          }

        }
      })
      Success(true)
    }

    private def createTreatmentItem(trialOCid: Int, trialOC: TrialOC, trial: Trial): Validation[String, Boolean] = {
      val fieldSet = trialOC.treatmentItem.getOrElse(return Failure("Treatment Item not defined"))
      val treatmentCriterion: OrdinalCriterion = fieldSet._4.criterion.asInstanceOf[OrdinalCriterion]
      val itemIdentifier = for {
        criterionId <- criterionDao.create(treatmentCriterion, trial.id)
        itemId <- createFieldSet(trialOCid, treatmentCriterion.copy(id = criterionId), fieldSet)
      } yield itemId
      itemIdentifier match {
        case Failure(failure) => Failure(failure)
        case Success(id) => {
          onDBWithTransaction {
            queryTrialOCFromId(trialOCid).mutate {
              r => r.row = r.row.copy(_7 = id)
            }
            Success(true)
          }
        }
      }
    }

    /**
     * returns id from Item
     */
    private def createFieldSet(trialOCid: Int, criterion: Criterion[_, Constraint[_]], fieldSet: (EventOC, FormOC, ItemGroupOC, ItemOC)): Validation[String, Int] = {
      for {
        eventId <- createAndGetEvenOC(trialOCid, fieldSet._1.oid)
        formId <- createAndGetFormOC(eventId, fieldSet._2.oid)
        itemGroupId <- createAndGetItemGroupOC(formId, fieldSet._3.oid)
        itemId <- createAndGetItemOC(itemGroupId, fieldSet._4, criterion)
      } yield itemId
    }

    def getId(trialOid: String): Validation[String, Int] = {
      onDB {
        queryTrialOCFromOid(trialOid).list match {
          case List() => getIdFromTrialSiteIdentifier(trialOid)
          case List(trialOC) => Success(trialOC._1)
          case _ => Failure("Duplicated trial oid")
        }
      }
    }

    private def getIdFromTrialSiteIdentifier(trialOid: String): Validation[String, Int] = {
      queryTrialOCFromTrialSiteOid(trialOid).list match {
        case List() => Failure("TrialOC not found")
        case List(trialID) => Success(trialID)
        case _ => Failure("Duplicated trial oid")
      }
    }

    private def getIdEventOc(trialOCid: Int, eventOid: String): Validation[String, Int] = {
      for (ts <- queryEventOCFromOidAndTrialOCid(eventOid, trialOCid)) return Success(ts._1)
      Failure("EventOC not found")
    }

    private def createAndGetEvenOC(trialOCid: Int, eventOid: String): Validation[String, Int] = {
      for {
        rowsChanged <- onDBWithTransaction { Success(EventOCs.noId insert (eventOid, trialOCid)) }
        id <- getIdEventOc(trialOCid, eventOid)
      } yield id
    }

    private def getIdFormOc(eventOCid: Int, formOid: String): Validation[String, Int] = {
      for (ts <- queryFormOCFromOidAndEventOCid(formOid, eventOCid)) return Success(ts._1)
      Failure("FormOC not found")
    }

    private def createAndGetFormOC(eventOCid: Int, formOid: String): Validation[String, Int] = {
      for {
        rowsChanged <- onDBWithTransaction { Success(FormOCs.noId insert (formOid, eventOCid)) }
        id <- getIdFormOc(eventOCid, formOid)
      } yield id
    }

    private def getIdItemGroupOc(formOCid: Int, itemGroupOid: String): Validation[String, Int] = {
      for (ts <- queryItemGroupOCFromOidAndFormOCid(itemGroupOid, formOCid)) return Success(ts._1)
      Failure("ItemGroupOC not found")
    }

    private def createAndGetItemGroupOC(formId: Int, itemGroupOid: String): Validation[String, Int] = {
      for {
        rowsChanged <- onDBWithTransaction { Success(ItemGroupOCs.noId insert (itemGroupOid, formId)) }
        id <- getIdItemGroupOc(formId, itemGroupOid)
      } yield id
    }

    private def getIdItemOc(itemGroupId: Int, itemOid: String): Validation[String, Int] = {
      for (ts <- queryItemOCFromOidAndItemGroupOCid(itemOid, itemGroupId)) return Success(ts._1)
      Failure("ItemOC not found")
    }

    private def createAndGetItemOC(itemGroupId: Int, itemOC: ItemOC, criterion: Criterion[_, Constraint[_]]): Validation[String, Int] = {
      val idItem = for {
        rowsChanged <- onDBWithTransaction { Success(ItemOCs.noId insert (itemOC.oid, itemGroupId, criterion.id)) }
        id <- getIdItemOc(itemGroupId, itemOC.oid)
      } yield id

      if (idItem.isSuccess && itemOC.ordinalValueMapping != null && !itemOC.ordinalValueMapping.isEmpty) {
        itemOC.ordinalValueMapping.iterator.foreach(keyValuePair => {
          onDBWithTransaction {
            Success(OrdinalValueMapping.noId insert (idItem.toOption.get, keyValuePair._1, keyValuePair._2))
          } match {
            case Failure(failure) => return Failure(failure)
            case _ =>
          }
        })
      }
      idItem

    }

    def updateTrialOC(trialOC: TrialOC): Validation[String, TrialOC] = {
      Failure("NYI")
    }

    def get(id: Int): Validation[String, TrialOC] = {
      onDB {
        val result = queryTrialOCFromId(id).list()
        if (result.isEmpty || result.size > 1) {
          Failure("TrialOC not found")
        } else {
          val trialOCRow = result.head

          val trial = trialDao.get(trialOCRow._6) match {
            case Failure(failure) => return Failure(failure)
            case Success(None) => return Failure("Trial not found.")
            case Success(Some(trialDB)) => trialDB
          }

          for {
            connection <- getConnection(id)
            trialSites <- getTrialSiteMapping(id)
            events <- getEventOCs(id, trial.criterions.asInstanceOf[List[Criterion[Any, Constraint[Any]]]])
            treatmentItemOID <- onDB { Success(queryItemOIDfromId(trialOCRow._7).list().head._2) }
            treatmentItem <- Success(findTreatmentFieldSet(events, treatmentItemOID))
            cleanTrial <- Success {
              if (treatmentItem.isDefined) {
                trial.copy(criterions = trial.criterions.filter(crit => crit.id != treatmentItem.get._4.criterion.id))
              } else trial
            }
            trialOC <- Success(TrialOC(trialOCRow._1, 0, trialOCRow._2, trialOCRow._3, trialOCRow._4, trial.description, trialOCRow._5, events, Some(cleanTrial), connection, treatmentItem, trialSites))
          } yield trialOC
        }
      }
    }

    private def findTreatmentFieldSet(events: List[EventOC], treatmentItemOID: String): Option[(EventOC, FormOC, ItemGroupOC, ItemOC)] = {

      for (event <- events) {
        for (form <- event.forms) {
          for (itemGroup <- form.items) {
            for (item <- itemGroup.items) {
              if (treatmentItemOID == item.oid) {
                return Some(event, form, itemGroup, item)
              }
            }
          }
        }
      }
      None
    }

    def getFromTrial(trial: Trial): Validation[String, TrialOC] = {
      onDB {
        val result = Query(TrialOCs).filter(trialOC => trialOC.trialId is trial.id).list()
        if (result.size == 1) {
          get(result.head._1)
        } else {
          Failure("TrialOC not found")
        }
      }
    }

    private def getConnection(trialOCid: Int): Validation[String, ConnectionOC] = {
      onDB {
        val result = queryConnectionOCFromTrialOCId(trialOCid).list()
        if (result.size == 1) {
          val connection = result.head
          Success(new ConnectionOC(connection._3, connection._4, connection._5, connection._6))
        } else {
          Failure("Connection not found")
        }
      }
    }

    private def getTrialSiteMapping(trialOCid: Int): Validation[String, Map[String, String]] = {
      onDB {
        val result = queryTrialSiteMapping(trialOCid).list()
        val tmpMap = mutable.HashMap[String, String]()
        result.foreach(site => tmpMap.put(site._3, site._4))
        Success(tmpMap.toMap)
      }
    }

    private def getEventOCs(trialOCId: Int, criterions: List[Criterion[Any, Constraint[Any]]]): Validation[String, List[EventOC]] = {
      onDB {
        val result = new ListBuffer[EventOC]()
        val events = Query(EventOCs).filter(row => row.trialOCId is trialOCId).list()
        events.foreach(event => {
          result.append(new EventOC(event._2, getFormOCs(event._1, criterions)))
        })
        Success(result.toList)
      }
    }

    private def getFormOCs(eventId: Int, criterions: List[Criterion[Any, Constraint[Any]]]): List[FormOC] = {
      val result = new ListBuffer[FormOC]()
      val forms = Query(FormOCs).filter(row => row.eventOCId is eventId).list()
      forms.foreach(form => {
        result.append(new FormOC(form._2, getItemGroupOCs(form._1, criterions)))
      })
      result.toList
    }

    private def getItemGroupOCs(formId: Int, criterions: List[Criterion[Any, Constraint[Any]]]): List[ItemGroupOC] = {
      val result = new ListBuffer[ItemGroupOC]()
      val itemGroups = Query(ItemGroupOCs).filter(row => row.formOCId is formId).list()
      itemGroups.foreach(itemGroup => {
        result.append(new ItemGroupOC(itemGroup._2, getItems(itemGroup._1, criterions)))
      })
      result.toList
    }

    private def getItems(itemGroupId: Int, criterions: List[Criterion[Any, Constraint[Any]]]): List[ItemOC] = {
      val result = new ListBuffer[ItemOC]()
      val items = Query(ItemOCs).filter(row => row.itemGroupOCId is itemGroupId).list()
      items.foreach(item => {
        val ordinalValuesRows = queryOrdinalValueMappingFromItemOC(item._1).list()
        val ordinalValues: Map[String, String] = if (!ordinalValuesRows.isEmpty) {
          ordinalValuesRows.map(row => (row._3, row._4)).toMap
        } else {
          Map()
        }
        result.append(new ItemOC(item._2, criterions.find(criterion => criterion.id == item._4).get, ordinalValues))
      })
      result.toList
    }

    def getAll(): Validation[String, List[TrialOC]] = {
      onDB {
        Success(allTrials.map(trialOCRow => {
          get(trialOCRow._1).toOption.getOrElse { return Failure("Error loding the local OpenClinica Trial") }
        }))
      }
    }

    def getAllLocalTrialOids(): Validation[String, List[String]] = {
      onDB {
        Success(allTrials.map(_._3))
      }
    }
  }

}