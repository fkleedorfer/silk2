Examples for using machine learning via Weka with Silk

To try an example, you need silk and weka (http://www.cs.waikato.ac.nz/ml/weka/)
Workflow:
1) run silk to create feature vectors: run silk
2) load feature vectors into weka, train a classifier (eg. classifiers/trees/j48) and store the model to ./classifier.model
3) run silk again to apply the model you just trained to find links

1) assuming you checked out the sources, navigate to silk2/silk-singlemachine and do

mvn exec:java \
    -Dexec.mainClass="de.fuberlin.wiwiss.silk.Silk" \
    -DconfigFile="../silk-core/src/main/resources/de/fuberlin/wiwiss/silk/linkspec/examples_ml/dbpedia_drugbank_drugs-createVectors.xml" \
    -DinstanceCacheDir="/tmp/instanceCache" \

as a result, the file ./vectors.csv is generated
NOTE: temporary data (instance cache) will be written to /tmp/instanceCache - the directory will be created if it does not exist

2)
    a) open Weka and load the file ./vectors.csv (from the Preprocess panel)
    b) train a classifier (in the Classify panel)
    c) when you are pleased with the classifier, save the model to ./classifier.model
       (via right-click on the respective entry in the ResultList)
    d) save the dataset (optionally after removing all instances) to ./vectors.arff
       This step is necessary so that the dataset structure can be read from within silk

3) again, assuming you checked out the sources, navigate to silk2/silk-singlemachine and do

mvn exec:java \
    -Dexec.mainClass="de.fuberlin.wiwiss.silk.Silk" \
    -DconfigFile="../silk-core/src/main/resources/de/fuberlin/wiwiss/silk/linkspec/examples_ml/dbpedia_drugbank_drugs-match.xml" \
    -DinstanceCacheDir="/tmp/instanceCache" \
    -Dreload=false

The resulting files
NOTE: temporary data (instance cache) will be written to /tmp/instanceCache - the directory will be created if it does not exist