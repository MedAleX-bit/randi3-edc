package org.randi3.edc.model.openClinica

import org.randi3.model.criterion.Criterion
import scala.collection.mutable.ListBuffer
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.model.{Entity, Trial}

case class TrialOC (val id: Int = Int.MinValue, val version: Int = 0, val identifier: String, val oid: String, val name: String, val description: String, val metaDataVersionOID: String = "", val events: List[EventOC] = List(), val trial: Option[Trial], connection: ConnectionOC) extends Entity{
 
  def getAllCriteria(): List[Criterion[Any, Constraint[Any]]] = {
    val result = new ListBuffer[Criterion[Any, Constraint[Any]]]
    //TODO refactor loops
    for(event <- events){ 
      for(forms <- event.forms){ 
	for(itemGroup <- forms.items){ 
	  for(item <- itemGroup.items){ 
	    result += item.criterion
	  }
	}
      }
    }
    result.toList
  }

  def getMappedElementsFromCriteria(criterion: Criterion[Any, Constraint[Any]]): Option[(EventOC, FormOC, ItemGroupOC, ItemOC)] ={

    val criteriaOid = criterion.name

    for(event <- events){
      for(form <- event.forms){
      for(itemGroup <- form.items){
        for(item <- itemGroup.items){
          if (criteriaOid == item.oid){
            return Some((event, form, itemGroup, item))
          }
        }
      }
    }
    }
    None
  }


}
