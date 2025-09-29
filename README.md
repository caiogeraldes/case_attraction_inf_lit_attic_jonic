# Database for Case Attraction in Infinitive Clauses in Literary Attic and Jonic Greek

**Author: Caio Geraldes**

**Supervisor: José Marcos Mariani de Macedo**

Version: `0.1.0` 

**NOTE:** This is a *pre-publication* version of the data.
There may be updates in the near future, which might include additions, 
deletions or changes of the values.
Once the main thesis to which this data is associated with is published, the
version `1.0.0` will be released.

This repository contains the data collected and annotated for the research 
[Semantics and pragmatics of ancient Greek case attraction](https://bv.fapesp.br/en/bolsas/198957/semantics-and-pragmatics-of-ancient-greek-case-attraction/),
funded by The São Paulo Research Foundation (Fapesp), grant number 21/06027-4.

The columns are annotated as follows:


- `ID`: sentence's unique ID
- `AUTHOR`: by name and TLG ID
- `WORK`: by name and TLG ID
- `LOCATION`: traditional numbering of the sentence in its book, chapter, verse or página Stephanus page
- `TEXT`: sentence's Unicode text
- `Vm`: matrix verb
- `VM_LEMMA`: matrix verb's lemma 
- `VM_PERSONAL`: boolean value denoting whether or not the matrix verb is
  personal
- `VM_MOD`: boolean value denoting whether or not the matrix verb has modal
  semantics
- `Vinf`: infinitive verb(s) of the sentence
- `VINF_A-D`: each infinitive verb in a separate column
- `Vinf_Cop`: boolean value denoting whether or not the infinitive verb is a
  copula
- `Obj`: oblique (dative or genitive) object of the matrix clause
- `OBJ_A-C`: each oblique object of the matrix verb
- `OBJ_TH`: assumed thematic role of `Obj`
- `Pred`: nominal predicate modifying the subject of the infinitive clause
- `PRED_A-G`: each nominal predicate in a separate column
- `pred_a_pos`: Part of Speech tag of `PRED_A`
- `Attr`: boolean value denoting whether or not the sentence has attraction on
  its first nominal predicate
- `Attr_B`: boolean value denoting whether or not the sentence has attraction on
  any other nominal predicate
- `DIST_OBJ_PRED`: distance between `OBJ_A` and `PRED_A`
- `GENRE`
- `DIALECT`
- `Variation`: manuscript variation of `Obj`, if existent
