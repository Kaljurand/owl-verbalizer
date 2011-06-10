Source:

* http://www.co-ode.org/galen/
* http://www.co-ode.org/galen/full-galen.owl (20 MB)
* Converted into OWL/XML with Protege 4.1 beta 231


This command takes 25 sec on i3:

./owl_to_ace.exe -xml ontologies/galen/full-galen.owl2xml.owl -format csv > ontologies/galen/full-galen.csv

With format = ace it takes 20 sec (which is surprising).


Verbalization statistics:

$ cat full-galen.csv | cut -f1 | sort | uniq -c | sort -nr
 146908 f
  72649 cn_sg
  36403 
  30562 tv_sg
  24101 ignored
    452 comment
