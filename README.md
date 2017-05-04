# copybookStreams
Avro schema derivation and data parsing from cobol copybook for Scala.

The pipeline can be explained as follows : 
The Scala App takes an COBOL copybook as input and tokenizes it to identify the
copybook schema. Then a parser takes the tokens generated from the copybook and
builds an AST(abstract syntax tree). The AST is then written to a .scala file. 
The generated .scala class file is used to create a AVRO schema. 
At this moment only the keys reside in the AVRO schema. The raw binary file is then
parsed with Scodec to human readable values. These values are then added to the 
correct corresponding key in the generic record. 

https://drive.google.com/a/zenaptix.com/file/d/0B5KPFloE62GlQWI4SjNRQlFXWUU/view?usp=sharing


## Build project
```$ sbt clean compile ```

## Run tests
```$ sbt test ```

