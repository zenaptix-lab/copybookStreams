import org.apache.avro.{Schema, SchemaBuilder}

val complete = SchemaBuilder
  .record("TheRecord").namespace("com.zenaptix.dsl")
  .fields()
  .name("L051").`type`("int").withDefault(8)
  .name("L052").`type`().record("l1")
  .fields()
  .name("L101").`type`("string").noDefault()
  .name("L053").`type`().record("l12")
  .fields()
  .name("L102").`type`("long").noDefault()
  .name("L103").`type`("string").noDefault()
  .endRecord()





val complete1 = SchemaBuilder
  .record("Mbsk861").namespace("com.zenaptix.dsll")
  .fields()
  .name("lngth861").`type`("int").noDefault()
  .name("pkeyk861").`type`().record("Pkeyk861")
  .fields()
  .name("uniqEftNoK861").`type`("string").noDefault()
  .name("datak861").`type`().record("Datak861")
  .fields()
  .name("incrPaymNoK861").`type`("long").noDefault()
  .name("tbcK861").`type`("string").noDefault()
  .endRecord()


println("completeSchema : " + complete1.noDefault().endRecord().noDefault().endRecord().toString(true))
