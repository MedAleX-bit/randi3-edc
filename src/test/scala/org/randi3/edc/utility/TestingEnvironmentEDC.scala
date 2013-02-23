package org.randi3.edc.utility

import org.randi3.schema.LiquibaseUtil
import org.randi3.edc.schema.OpenClinicaDatabaseSchema
import org.randi3.utility.TestingEnvironment
import org.randi3.edc.dao.OpenClinicaDaoComponent
import org.randi3.edc.model.openClinica._
import org.randi3.model.criterion.constraint.{Constraint, DoubleConstraint}
import org.randi3.model.criterion.{Criterion, IntegerCriterion, DoubleCriterion}
import org.randi3.edc.model.openClinica.ConnectionOC
import org.randi3.edc.model.openClinica.ItemOC
import org.randi3.edc.model.openClinica.TrialOC
import org.randi3.edc.model.openClinica.ItemGroupOC
import scala.Some
import org.randi3.edc.service.OpenClinicaServiceComponent

object TestingEnvironmentEDC extends TestingEnvironment with OpenClinicaDaoComponent with OpenClinicaServiceComponent{


  val openClinicaSchema = new OpenClinicaDatabaseSchema(driver)

  LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-edc.xml", this.getClass.getClassLoader)

   lazy val openClinicaDao = new OpenClinicaDao

  val openClinicaService = new OpenClinicaService

  private val inclusionConstraint = Some(DoubleConstraint(configurations = List(Some(1.0), Some(2.0))).toOption.get)
  private val criterion1: DoubleCriterion = DoubleCriterion(name = "name1", description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
  private val criterion2: IntegerCriterion = IntegerCriterion(name = "name2", description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get

  private val connectionOC = new ConnectionOC(location = "localhost", username = "xyz", passwordHash = "abc")

  val item1 = new ItemOC(oid = "name1", criterion = criterion1.asInstanceOf[Criterion[Any, Constraint[Any]]])
  val item2 = new ItemOC(oid = "name2", criterion = criterion2.asInstanceOf[Criterion[Any, Constraint[Any]]])

  val itemGroup = new ItemGroupOC(oid = "itemGroupOid", List(item1, item2))

  val form = new FormOC(oid = "formOid", items = List(itemGroup))
  val event = new EventOC(oid = "eventOid", forms = List(form))


  def getTrialOC = new TrialOC(name = "testTrialOC", oid ="oid", description = "description", identifier = "testIdentifier", connection = connectionOC, trial = Some(createTrial.copy(criterions = List(criterion1, criterion2))), events = List(event))

}