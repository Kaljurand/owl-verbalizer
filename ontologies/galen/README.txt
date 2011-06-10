Source:

* http://www.co-ode.org/galen/
* http://www.co-ode.org/galen/full-galen.owl (20 MB)
* Converted into OWL/XML with Protege 4.1 beta 231


This command takes 9 sec on i3:

./owl_to_ace.exe -xml ontologies/galen/full-galen.owl2xml.owl -format csv > ontologies/galen/full-galen.csv

With formats 'ace' and 'html' it takes 32 sec.
These outputs take longer to generate becuse in both cases
surface forms are generated (which means concat_atom/3).


Verbalization statistics:

$ cat full-galen.csv | cut -f1 | sort | uniq -c | sort -nr

 251766 f
 120702 cn_sg
  63339 
  47168 tv_sg
  25643 ignored
    456 comment
    337 tv_vbg

i.e.:

37696 logical axioms out of which 456 were not verbalized
(this is consistent with the chapter in the thesis)
