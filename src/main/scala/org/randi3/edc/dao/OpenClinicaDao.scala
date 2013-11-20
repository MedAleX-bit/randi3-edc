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
        trialService.create(trialOC.trial.getOrElse(return Failure("Trial must be set")).copy(isEDCTrial = true, status = TrialStatus.ACTIVE), currentUser.get).toEither match {
          case Left(failure) => Failure(failure)
          case Right(trialId) => {

            threadLocalSession withTransaction {
              TrialOCs.noId insert (trialOC.identifier, trialOC.oid, trialOC.name, trialOC.metaDataVersionOID, trialId)
            }
            val trial = trialDao.get(trialId).toOption.get.get //TODO
            getId(trialOC.oid).toEither match {
              case Left(failure) => Failure(failure)
              case Right(trialOCid) => {
                createItems(trialOCid, trialOC, trial)
                createTreatmentItem(trialOCid, trialOC, trial)
                createTrialSiteMapping(trialOCid, trialOC)
                threadLocalSession withTransaction {
                  Connections.noId insert (trialOCid, trialOC.connection.location, trialOC.connection.username, trialOC.connection.passwordHash, trialOC.connection.dataSetId)
                }
                get(trialOCid)
              }
            }
          }
        }
      }

    }

    private def createTrialSiteMapping(trialOCid: Int, trialOC: TrialOC): Validation[String, Boolean] = {
      TrialSiteMapping.noId insertAll (trialOC.sites.map(site => (trialOCid, site._1, site._2)).toSeq: _*)
      Success(true)
    }

    private def createItems(trialOCid: Int, trialOC: TrialOC, trial: Trial): Validation[String, Boolean] = {
      trial.criterions.foreach(criterion => {
        trialOC.getMappedElementsFromCriteria(criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]) match {
          case None => return Failure("Criterion in OpenClinica trial not found")
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
      criterionDao.create(treatmentCriterion, trial.id).toOption match {
        case None => return Failure("Couldn't create Treatment criterion")
        case Some(criterionId) => {
          createFieldSet(trialOCid, treatmentCriterion.copy(id = criterionId), fieldSet).toEither match {
            case Left(failure) => return Failure(failure)
            case Right(itemId) => {
              threadLocalSession withTransaction {
                queryTrialOCFromId(trialOCid).mutate {
                  r =>
                    r.row = r.row.copy(_7 = itemId)
                }
              }
            }
          }
        }
      }
      Success(true)
    }

    /**
     * returns id from Item
     */
    private def createFieldSet(trialOCid: Int, criterion: Criterion[_, Constraint[_]], fieldSet: (EventOC, FormOC, ItemGroupOC, ItemOC)): Validation[String, Int] = {
      threadLocalSession withTransaction {
        EventOCs.noId insert (fieldSet._1.oid, trialOCid)
      }
      getIdEventOc(trialOCid, fieldSet._1.oid).toEither match {
        case Left(failure) => return Failure(failure)
        case Right(eventId) => {
          threadLocalSession withTransaction {
            FormOCs.noId insert (fieldSet._2.oid, eventId)
          }
          getIdFormOc(eventId, fieldSet._2.oid).toEither match {
            case Left(failure) => return Failure(failure)
            case Right(formId) => {
              threadLocalSession withTransaction {
                ItemGroupOCs.noId insert (fieldSet._3.oid, formId)
              }
              getIdItemGroupOc(formId, fieldSet._3.oid).toEither match {
                case Left(failure) => return Failure(failure)
                case Right(itemGroupId) => {
                  threadLocalSession withTransaction {
                    ItemOCs.noId insert (fieldSet._4.oid, itemGroupId, criterion.id)
                  }
                  val idItem = getIdItemOc(itemGroupId, fieldSet._4.oid)

                  if (idItem.isSuccess && fieldSet._4.ordinalValueMapping != null && !fieldSet._4.ordinalValueMapping.isEmpty) {
                    fieldSet._4.ordinalValueMapping.iterator.foreach(keyValuePair => {
                      OrdinalValueMapping.noId insert (idItem.toOption.get, keyValuePair._1, keyValuePair._2)
                    })
                  }
                  idItem
                }
              }
            }
          }
        }
      }
    }

    def getId(trialOid: String): Validation[String, Int] = {
      onDB {
        queryTrialOCFromOid(trialOid).list match {
          case List() => getIdFromTrialSiteIdentifier(trialOid)
          case List(trialOC) => Success(trialOC._1)
          case _ => println("Duplicated trial oid"); Failure("Duplicated trial oid")
        }
      }
    }

    private def getIdFromTrialSiteIdentifier(trialOid: String): Validation[String, Int] = {
      queryTrialOCFromTrialSiteOid(trialOid).list match {
        case List() => Failure("TrialOC not found")
        case List(trialID) => Success(trialID)
        case _ => println("Duplicated trial oid"); Failure("Duplicated trial oid")
      }
    }

    private def getIdEventOc(trialOCid: Int, eventOid: String): Validation[String, Int] = {
      for (ts <- queryEventOCFromOidAndTrialOCid(eventOid, trialOCid)) return Success(ts._1)
      Failure("EventOC not found")
    }

    private def getIdFormOc(eventOCid: Int, formOid: String): Validation[String, Int] = {
      for (ts <- queryFormOCFromOidAndEventOCid(formOid, eventOCid)) return Success(ts._1)
      Failure("FormOC not found")
    }

    private def getIdItemGroupOc(formOCid: Int, itemGroupOid: String): Validation[String, Int] = {
      for (ts <- queryItemGroupOCFromOidAndFormOCid(itemGroupOid, formOCid)) return Success(ts._1)
      Failure("ItemGroupOC not found")
    }

    private def getIdItemOc(itemGroupOid: Int, itemOid: String): Validation[String, Int] = {
      for (ts <- queryItemOCFromOidAndItemGroupOCid(itemOid, itemGroupOid)) return Success(ts._1)
      Failure("ItemOC not found")
    }

    def updateTrialOC(trialOC: TrialOC): Validation[String, TrialOC] = {
      Failure("NYI")
    }

    def get(id: Int): Validation[String, TrialOC] = {
      onDB {
        val result = queryTrialOCFromId(id).list()
        if (result.size == 1) {
          val trialOCRow = result.head

          trialDao.get(trialOCRow._6).toEither match {
            case Left(failure) => Failure(failure)
            case Right(None) => Failure("Trial not found")
            case Right(Some(trial)) => {
              getConnection(id).toEither match {
                case Left(failure) => Failure("Connection not found: " + failure)
                case Right(connection) => {
                  getTrialSiteMapping(id).toEither match {
                    case Left(failure) => Failure("TrialSite Mapping failure:" + failure)
                    case Right(trialSites) => {
                      getEventOCs(id, trial.criterions.asInstanceOf[List[Criterion[Any, Constraint[Any]]]]).toEither match {
                        case Left(failure) => Failure("Failure with EventOC: " + failure)
                        case Right(events) => {
                          val treatmentItemOID = queryItemOIDfromId(trialOCRow._7).list().head._2
                          val treatmentItem = findTreatmentFieldSet(events, treatmentItemOID)
                          val cleanTrial = if (treatmentItem.isDefined) {
                            trial.copy(criterions = trial.criterions.filter(crit => crit.id != treatmentItem.get._4.criterion.id))
                          } else trial
                          Success(TrialOC(trialOCRow._1, 0, trialOCRow._2, trialOCRow._3, trialOCRow._4, trial.description, trialOCRow._5, events, Some(cleanTrial), connection, treatmentItem, trialSites))
                        }
                      }
                    }
                  }

                }
              }

            }
          }
        } else {
          Failure("TrialOC not found")
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
          get(trialOCRow._1).toOption.getOrElse(return Failure("Error loding the local OpenClinica Trial"))
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