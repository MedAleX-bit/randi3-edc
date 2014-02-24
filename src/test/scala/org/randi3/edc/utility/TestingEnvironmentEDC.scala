package org.randi3.edc.utility

import org.randi3.schema.{DatabaseSchema, LiquibaseUtil}
import org.randi3.edc.schema.OpenClinicaDatabaseSchema
import org.randi3.utility.TestingEnvironment
import org.randi3.edc.dao.OpenClinicaDaoComponent
import org.randi3.edc.model.openClinica._
import org.randi3.model.criterion.constraint.{Constraint, DoubleConstraint}
import org.randi3.model.criterion.{Criterion, IntegerCriterion, DoubleCriterion, OrdinalCriterion}
import org.randi3.edc.model.openClinica.ConnectionOC
import org.randi3.edc.model.openClinica.ItemOC
import org.randi3.edc.model.openClinica.TrialOC
import org.randi3.edc.model.openClinica.ItemGroupOC
import scala.Some
import org.randi3.edc.service.OpenClinicaServiceComponent

object TestingEnvironmentEDC extends TestingEnvironment with OpenClinicaDaoComponent with OpenClinicaServiceComponent{

 // override val databaseTuple: (Database, ExtendedProfile) = getDatabasePostgreSQL(properties.getProperty("testDatabaseName"), properties.getProperty("testDatabaseUser"), properties.getProperty("testDatabasePassword"))


  val openClinicaSchema = new OpenClinicaDatabaseSchema(driver)

  LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-edc.xml", this.getClass.getClassLoader)



  val criterionDoa = new CriterionDao

   lazy val openClinicaDao = new OpenClinicaDao

  val openClinicaService = new OpenClinicaService

  private val inclusionConstraint = Some(DoubleConstraint(configurations = List(Some(1.0), Some(2.0))).toOption.get)
  def criterion1(name: String): DoubleCriterion = DoubleCriterion(name = name, description = "descritpion", inclusionConstraint = inclusionConstraint, strata = Nil).toOption.get
  def criterion2(name: String): IntegerCriterion = IntegerCriterion(name = name, description = "descritpion", inclusionConstraint = None, strata = Nil).toOption.get

  private val connectionOC = new ConnectionOC(location = "localhost", username = "xyz", passwordHash = "abc", 1)

  def item1 = {
    val name = "name1" + random.nextLong()
    new ItemOC(oid = name, criterion = criterion1(name).asInstanceOf[Criterion[Any, Constraint[Any]]])
  }
  def item2 =  {
    val name = "name2" + random.nextLong()
    new ItemOC(oid = name, criterion = criterion2(name).asInstanceOf[Criterion[Any, Constraint[Any]]])
  }
  def itemGroup = new ItemGroupOC(oid = "itemGroupOid"+ random.nextLong(), List(item1, item2))

  def form = new FormOC(oid = "formOid" + random.nextLong(), items = List(itemGroup))
  def event = new EventOC(oid = "eventOid"+ random.nextLong(), forms = List(form))

  //TODO treatment item
  
  def getTrialOC = {
    val actEvent = event
    val crit1 = actEvent.forms.head.items.head.items.head.criterion
    val crit2 = actEvent.forms.head.items.head.items.last.criterion
    val trialAct = createTrial.copy(criterions = List(crit1, crit2))
    val treatmentCriterion = OrdinalCriterion(name ="treatmentProp"+ random.nextDouble() , description ="description", values =  trialAct.treatmentArms.map(arm => arm.name).toSet, inclusionConstraint = None, strata = List()).toOption.get
    val treatmentItem = new ItemOC(oid = treatmentCriterion.name, criterion = treatmentCriterion.asInstanceOf[Criterion[Any, Constraint[Any]]]) 
    val treatmentGroup = new ItemGroupOC(oid = "itemGroupOid"+ random.nextLong(), List(treatmentItem))
    val treatmentForm = new FormOC(oid = "formOid" + random.nextLong(), items = List(treatmentGroup))
    val treatmentEvent = new EventOC(oid = "eventOid"+ random.nextLong(), forms = List(treatmentForm))
    val treatmentReferenz = (treatmentEvent, treatmentForm, treatmentGroup, treatmentItem)
    new TrialOC(name = "testTrialOC"+ random.nextLong(), oid ="oid"+ random.nextLong(), description = "description", identifier = "testIdentifier"+ random.nextLong(), connection = connectionOC, trial = Some(trialAct), events = List(actEvent), treatmentItem = Some(treatmentReferenz))
  }

}