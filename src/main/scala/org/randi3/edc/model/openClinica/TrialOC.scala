package org.randi3.edc.model.openClinica

import org.randi3.model.criterion.Criterion
import scala.collection.mutable.ListBuffer
import org.randi3.model.criterion.constraint.Constraint

case class TrialOC (val identifier: String, val oid: String, val name: String, val metaDataVersionOID: String = "", val events: List[EventOC] = List()){
 
  def getAllCriteria(): List[Criterion[_ <: Any, Constraint[_ <: Any]]] = {
    val result = new ListBuffer[Criterion[_ <: Any, Constraint[_ <: Any]]]
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
}
