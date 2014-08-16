type clause = R of rule | F of fact
and
fact = Head of atom
and
rule = Headbody of atom*(atom list)
and
atom = V of name|NZ of name*(atom list)|Z of name|Not of atom
and
name=string;;

                         
  