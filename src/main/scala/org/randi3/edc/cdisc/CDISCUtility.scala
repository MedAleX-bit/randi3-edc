package org.randi3.openclinica.cdisc


import org.randi3.model.SubjectProperty
import org.randi3.model.Trial
import org.randi3.model.TrialSubject
import scala.xml.NodeSeq
import org.randi3.model.criterion._
import constraint.Constraint
import scala.annotation.tailrec
import org.randi3.edc.model.openClinica._
import scala.Some
import org.randi3.edc.model.openClinica.ItemOC
import org.randi3.edc.model.openClinica.FormOC
import org.randi3.edc.model.openClinica.ItemGroupOC
import org.randi3.edc.model.openClinica.EventOC

object CDISCUtility {

  def getFullTrialFromODM(odm: NodeSeq): Option[TrialOC] = {
    val oid = ((odm \\ "Study") \ "@OID").text
    val name = (odm \\ "StudyName").text
    val identifier = (odm \\ "ProtocolName").text
    Some(new TrialOC(oid, name, identifier, metaDataVersionOID = ((odm \\ "MetaDataVersion")(0) \ "@OID").text, events = extractEvents((odm \\ "StudyEventDef"), odm)))
  }

  private def extractEvents(eventSeq: NodeSeq, fullSeq: NodeSeq): List[EventOC] = {
    @tailrec def iter(eventSeq: NodeSeq, fullSeq: NodeSeq, acc: List[EventOC]): List[EventOC] = {
      if (eventSeq.isEmpty) return acc
      iter(eventSeq.tail, fullSeq, new EventOC(oid = (eventSeq.head \ "@OID").text, forms = extractForms((eventSeq.head \ "FormRef"), fullSeq)) :: acc)
    }
    iter(eventSeq, fullSeq, Nil)
  }

  // TODO TailRec
  private def extractForms(formSeq: NodeSeq, fullSeq: NodeSeq): List[FormOC] = {
    if (formSeq.isEmpty) return List()
    val oid = (formSeq.head \ "@FormOID").text
    val itemGroupSeq = ((fullSeq \\ "FormDef").filter(formDef => (formDef \ "@OID").text == oid) \\ "ItemGroupRef")
    new FormOC(oid, extractItemGroups(itemGroupSeq, fullSeq)) :: extractForms(formSeq.tail, fullSeq)
  }

  // TODO TailRec
  private def extractItemGroups(itemGroupSeq: NodeSeq, fullSeq: NodeSeq): List[ItemGroupOC] = {
    if (itemGroupSeq.isEmpty) return List()
    val oid = (itemGroupSeq.head \ "@ItemGroupOID").text
    val itemSeq = ((fullSeq \\ "ItemGroupDef").filter(itemGroupDef => (itemGroupDef \ "@OID").text == oid) \ "ItemRef")
    new ItemGroupOC(oid, extractItems(itemSeq, fullSeq)) :: extractItemGroups(itemGroupSeq.tail, fullSeq)
  }

  // TODO TailRec
  private def extractItems(itemSeq: NodeSeq, fullSeq: NodeSeq): List[ItemOC] = {
    if (itemSeq.isEmpty) return List()
    val head: NodeSeq = itemSeq.head
    val oid = (head \ "@ItemOID").text
    val singleItemSeq = ((fullSeq \\ "ItemDef").filter(itemDef => (itemDef \ "@OID").text == oid))
    val criterion: Criterion[Any, Constraint[Any]] =
      if (!(singleItemSeq \\ "CodeListRef").isEmpty) {
        extractOrdinalCriterion(singleItemSeq, fullSeq).asInstanceOf[Criterion[Any, Constraint[Any]]]
      } else if (!(singleItemSeq \\ "MultiSelectListRef").isEmpty) {
        //Do nothing, cant't handle multi selectable elements
        null
      } else {
        extractSimpleCriterion(singleItemSeq).asInstanceOf[Criterion[Any, Constraint[Any]]]
      }
    val itemOC = if (criterion == null) Nil else List(new ItemOC(oid, criterion))
    itemOC ::: extractItems(itemSeq.tail, fullSeq)
  }

  private def extractOrdinalCriterion(itemSeq: NodeSeq, fullSeq: NodeSeq): Criterion[_ <: Any, Constraint[_ <: Any]] = {
    val name = (itemSeq \ "@Name").text
    val description = (itemSeq \ "@Comment").text
    val codeListOid = ((itemSeq \\ "CodeListRef") \ "@CodeListOID").text
    val codeListRef = (fullSeq \\ "CodeList").filter(codeList => (codeList \ "@OID").text == codeListOid)
    OrdinalCriterion(name = name, description = description, values = extractCodeListItems(codeListRef), inclusionConstraint = None, strata = Nil).toOption.get
  }

  private def extractCodeListItems(codeSeq: NodeSeq): Set[String] = {
    @tailrec def extractCodeValue(codeValueSeq: NodeSeq, acc: List[String]): Set[String] = {
      if (codeValueSeq.isEmpty) return acc.toSet
      val value = (codeValueSeq.head \ "@CodedValue").text
      extractCodeValue(codeValueSeq.tail, value :: acc)
    }
    extractCodeValue((codeSeq \\ "CodeListItem"), Nil)
  }

  private def extractSimpleCriterion(itemSeq: NodeSeq): Criterion[_ <: Any, Constraint[_ <: Any]] = {
    val itemOid = ((itemSeq \ "ItemDetails") \ "@ItemOID").text
    val description = (itemSeq \ "@Comment").text
    (itemSeq \ "@DataType").text match {
      case "text" => FreeTextCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      case "integer" => IntegerCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      case "float" => DoubleCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      case "data" | "partialDate" => DateCriterion(name = itemOid, description = description, inclusionConstraint = None, strata = Nil).toOption.get
      case _ => null
    }
  }

  def getAllSubjectsFromODM(odm: NodeSeq, trial: Trial): List[TrialSubject] = {
    extractSubjects(odm \\ "SubjectData", trial.criterions)
  }

  private def extractSubjects(subjectNodes: NodeSeq, criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]]): List[TrialSubject] = {
    if (subjectNodes.isEmpty) return List()
    val subjectProperties =  extractSubjectProperties(subjectNodes.head \\ "ItemData", criterions)
    val identifier = (subjectNodes.head \ "@SubjectKey").text
    val subject = TrialSubject(identifier = identifier, investigatorUserName = "", properties = subjectProperties, trialSite = null).toOption.get
    subject :: extractSubjects(subjectNodes.tail, criterions)
  }

  private def extractSubjectProperties(itemNodes: NodeSeq, criterions: List[Criterion[_ <: Any, Constraint[_ <: Any]]]): List[SubjectProperty[Any]] =  {
    if (itemNodes.isEmpty) return List()
    val itemOID = (itemNodes.head \ "@ItemOID").text
    val value = (itemNodes.head \ "@Value").text
    //if no criterion with the itemOid exists try next itemNode
    val criterion = criterions.find(crit => crit.name == itemOID).orElse( return extractSubjectProperties(itemNodes.tail, criterions)).get.asInstanceOf[Criterion[Any, Constraint[Any]]]
    SubjectProperty(criterion = criterion, value = value).toOption.get :: extractSubjectProperties(itemNodes.tail, criterions)
  }

}
