package org.randi3.edc.dao

import org.randi3.utility.{UtilityDBComponent, Logging}
import scalaz.{Success, Failure, Validation}
import org.randi3.edc.model.openClinica._
import org.randi3.dao.{TrialDaoComponent, DaoComponent}
import org.scalaquery.session.Database._
import org.scalaquery.ql.{Query, Parameters}
import scala.Left
import scalaz.Failure
import scala.Right
import scalaz.Success
import org.randi3.edc.model.openClinica.FormOC
import org.randi3.edc.model.openClinica.TrialOC
import org.randi3.edc.model.openClinica.ItemGroupOC
import org.randi3.edc.model.openClinica.EventOC
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import collection.mutable.ListBuffer
import org.randi3.edc.schema.OpenClinicaDatabaseSchema
import org.randi3.model.Trial


trait OpenClinicaDaoComponent {

  this: DaoComponent with
    Logging with
    UtilityDBComponent with
    TrialDaoComponent =>

  val openClinicaSchema: OpenClinicaDatabaseSchema
  val openClinicaDao: OpenClinicaDao

  class OpenClinicaDao {

    import driver.Implicit._
    import schema._
    import openClinicaSchema._
    import utilityDB._


    private val queryTrialOCFromOid = for {
      oid <- Parameters[String]
      trial <- TrialOCs if trial.oid is oid
    } yield trial

    private val queryTrialOCFromId = for {
      id <- Parameters[Int]
      trial <- TrialOCs if trial.id is id
    } yield trial


    private val queryConnectionOCFromTrialOCId = for {
      trialOCid <- Parameters[Int]
      connection <- Connections if connection.trialOCId is trialOCid
    } yield connection

    private def queryEventOCFromOidAndTrialOCid(oid: String, trialOCid: Int) = for {
      event <- EventOCs if event.trialOCId === trialOCid && event.oid === oid
    } yield event

    private def queryFormOCFromOidAndEventOCid(oid: String, eventOCid: Int) = for {
      form <- FormOCs if form.eventOCId === eventOCid && form.oid === oid
    } yield form

    private def queryItemGroupOCFromOidAndFormOCid(oid: String, formOCid: Int) = for {
      itemGroup <- ItemGroupOCs if itemGroup.formOCId === formOCid && itemGroup.oid === oid
    } yield itemGroup


    def create(trialOC: TrialOC): Validation[String, TrialOC] = {
      onDB {
          trialDao.create(trialOC.trial.getOrElse(return Failure("Trial must be set"))).either match {
            case Left(failure) => Failure(failure)
            case Right(trialId) => {
              threadLocalSession withTransaction {
                TrialOCs.noId insert(trialOC.identifier, trialOC.oid, trialOC.name, trialOC.metaDataVersionOID, trialId)
              }
              val trial = trialDao.get(trialId).toOption.get.get //TODO
              getId(trialOC.oid).either match {
                case Left(failure) => Failure(failure)
                case Right(trialOCid) => {
                  createEventOCs(trialOCid, trialOC.events, trial)
                  threadLocalSession withTransaction {
                    Connections.noId insert(trialOCid, trialOC.connection.location, trialOC.connection.username, trialOC.connection.passwordHash)
                  }
                  get(trialOCid)
                }
              }
            }
          }
      }

    }

    private def getId(trialOid: String): Validation[String, Int] = {
      for (ts <- queryTrialOCFromOid(trialOid)) return Success(ts._1)
      Failure("TrialOC not found")
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


    private def createEventOCs(trialOCid: Int, eventOCs: List[EventOC], trial: Trial) {
      eventOCs.foreach(event => {
        threadLocalSession withTransaction {
          EventOCs.noId insert(event.oid, trialOCid)
        }
        getIdEventOc(trialOCid, event.oid).either match {
          case Left(failure) => println("event" + failure)//TODO
          case Right(eventId) => {
            createFormOCs(eventId, event.forms, trial)
          }
        }
      })
    }

    private def createFormOCs(eventId: Int, formOCs: List[FormOC], trial: Trial) {
      formOCs.foreach(form => {
        threadLocalSession withTransaction {
          FormOCs.noId insert(form.oid, eventId)
        }
        getIdFormOc(eventId, form.oid).either match {
          case Left(failure) => println(failure) //TODO
          case Right(eventId) => {
            createItemGroupOCs(eventId, form.items, trial)
          }
        }
      })
    }

    private def createItemGroupOCs(formId: Int, itemGroupOCs: List[ItemGroupOC], trial: Trial) {
      itemGroupOCs.foreach(itemGroup => {
        threadLocalSession withTransaction {
          ItemGroupOCs.noId insert(itemGroup.oid, formId)
        }
        getIdItemGroupOc(formId, itemGroup.oid).either match {
          case Left(failure) => println(failure)//TODO
          case Right(itemGroupId) => {
            createItemOCs(itemGroupId, itemGroup.items, trial)
          }
        }
      })
    }

    private def createItemOCs(itemGroupId: Int, itemOCs: List[ItemOC], trial: Trial) {
      itemOCs.foreach(item => {
        threadLocalSession withTransaction {
          val criterionId = trial.criterions.find(criterion => criterion.name == item.criterion.name).get.id
          ItemOCs.noId insert(item.oid, itemGroupId, criterionId)
        }
      })
    }

    def updateTrialOC(trialOC: TrialOC): Validation[String, TrialOC] = {

      Failure("NYI")
    }


    def get(id: Int): Validation[String, TrialOC] = {
      onDB {
        val result = queryTrialOCFromId(id).list()
        if (result.size == 1) {
          val trialOCRow = result.head

          trialDao.get(trialOCRow._6).either match {
            case Left(failure) => Failure(failure)
            case Right(None) => Failure("Trial not found")
            case Right(Some(trial)) => {
              getConnection(id).either match {
                case Left(failure) => Failure("Connection not found: " + failure)
                case Right(connection) => {
                  getEventOCs(id, trial.criterions.asInstanceOf[List[Criterion[Any, Constraint[Any]]]]).either match {
                    case Left(failure) => Failure("Failure with EventOC: " + failure)
                    case Right(events) => {
                      Success(TrialOC(trialOCRow._1, 0, trialOCRow._2, trialOCRow._3, trialOCRow._4, trial.description, trialOCRow._5, events, Some(trial), connection))
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


    def getFromTrial(trial: Trial): Validation[String, TrialOC] = {
      onDB{
        val result = Query(TrialOCs).filter(trialOC => trialOC.trialId is trial.id).list()
        if(result.size ==1) {
          get(result.head._1)
        } else  {
          Failure("TrialOC not found")
        }
      }
    }

    private def getConnection(trialOCid: Int): Validation[String, ConnectionOC] = {
      onDB {
        val result = queryConnectionOCFromTrialOCId(trialOCid).list()
        if (result.size == 1) {
          val connection = result.head
          Success(new ConnectionOC(connection._3, connection._4, connection._5))
        } else {
          Failure("Connection not found")
        }
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
        result.append(new ItemOC(item._2, criterions.find(criterion => criterion.id == item._4).get))
      })
      result.toList
    }


    def getAll(): Validation[String, List[TrialOC]] = {

      Failure("NYI")
    }


  }

}