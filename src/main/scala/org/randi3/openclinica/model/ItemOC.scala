package org.randi3.openclinica.model
import org.randi3.model.criterion.Criterion

case class ItemOC (val oid: String, val criterion: Criterion[Any]){

}