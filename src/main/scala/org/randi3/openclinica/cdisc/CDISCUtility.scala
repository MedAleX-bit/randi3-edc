package org.randi3.openclinica.cdisc


import org.randi3.model.SubjectProperty
import org.randi3.model.Trial
import org.randi3.model.TrialSubject
import org.randi3.openclinica.model.TrialOC
import scala.xml.NodeSeq
import org.randi3.openclinica.model.EventOC
import org.randi3.model.criterion._
import org.randi3.openclinica.model.ItemOC
import org.randi3.openclinica.model.ItemGroupOC
import org.randi3.openclinica.model.FormOC
import scala.annotation.tailrec

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
    val criterion =
      if (!(singleItemSeq \\ "CodeListRef").isEmpty) {
        extractOrdinalCriterion(singleItemSeq, fullSeq)
      } else if (!(singleItemSeq \\ "MultiSelectListRef").isEmpty) {
        //Do nothing, cant't handle multi selectable elements
        null
      } else {
        extractSimpleCriterion(singleItemSeq)
      }
    val itemOC = if (criterion == null) Nil else List(new ItemOC(oid, criterion))
    itemOC ::: extractItems(itemSeq.tail, fullSeq)
  }

  private def extractOrdinalCriterion(itemSeq: NodeSeq, fullSeq: NodeSeq): Criterion[Any] = {
    val name = (itemSeq \ "@Name").text
    val description = (itemSeq \ "@Comment").text
    val codeListOid = ((itemSeq \\ "CodeListRef") \ "@CodeListOID").text
    val codeListRef = (fullSeq \\ "CodeList").filter(codeList => (codeList \ "@OID").text == codeListOid)
    new OrdinalCriterion(name = name, description = description, values = extractCodeListItems(codeListRef))
  }

  private def extractCodeListItems(codeSeq: NodeSeq): Set[String] = {
    @tailrec def extractCodeValue(codeValueSeq: NodeSeq, acc: List[String]): Set[String] = {
      if (codeValueSeq.isEmpty) return acc.toSet
      val value = (codeValueSeq.head \ "@CodedValue").text
      extractCodeValue(codeValueSeq.tail, value :: acc)
    }
    extractCodeValue((codeSeq \\ "CodeListItem"), Nil)
  }

  private def extractSimpleCriterion(itemSeq: NodeSeq): Criterion[Any] = {
    val itemOid = ((itemSeq \ "ItemDetails") \ "@ItemOID").text
    val description = (itemSeq \ "@Comment").text
    (itemSeq \ "@DataType").text match {
      case "text" => new FreeTextCriterion(name = itemOid, description = description)
      case "integer" => new IntegerCriterion(name = itemOid, description = description)
      case "float" => new DoubleCriterion(name = itemOid, description = description)
      case "data" | "partialDate" => new DateCriterion(name = itemOid, description = description)
      case _ => null
    }
  }

  def getAllSubjectsFromODM(odm: NodeSeq, trial: Trial): List[TrialSubject] = {
    extractSubjects(odm \\ "SubjectData", trial.criterions)
  }

  private def extractSubjects(subjectNodes: NodeSeq, criterions: List[Criterion[Any]]): List[TrialSubject] = {
    if (subjectNodes.isEmpty) return List()
    val subjectProperties =  extractSubjectProperties(subjectNodes.head \\ "ItemData", criterions)
    val identifier = (subjectNodes.head \ "@SubjectKey").text
    val subject = new TrialSubject(identifier = identifier, properties = subjectProperties)
    subject :: extractSubjects(subjectNodes.tail, criterions)
  }

  private def extractSubjectProperties(itemNodes: NodeSeq, criterions: List[Criterion[Any]]): List[SubjectProperty[Any]] =  { 
    if (itemNodes.isEmpty) return List()
    val itemOID = (itemNodes.head \ "@ItemOID").text
    val value = (itemNodes.head \ "@Value").text
    //if no criterion with the itemOid exists try next itemNode
    val criterion = criterions.find(crit => crit.name == itemOID).orElse( return extractSubjectProperties(itemNodes.tail, criterions)).get
    new SubjectProperty(criterion = criterion, value = value) :: extractSubjectProperties(itemNodes.tail, criterions)
  }

}
