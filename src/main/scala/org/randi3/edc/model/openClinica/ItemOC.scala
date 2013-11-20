package org.randi3.edc.model.openClinica

import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint

case class ItemOC (val oid: String, val criterion: Criterion[Any, Constraint[Any]] , val ordinalValueMapping: Map[String, String] = Map()){

}