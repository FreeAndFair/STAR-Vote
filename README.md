STAR-Vote
===

*STAR-Vote* is part of the Verifiable Elections software suite of products, developed by Verifiable Elections, Inc. It is a *S*ecure, *T*ransparent, *A*uditable and *R*eliable voting system that runs on off-the-shelf computing hardware, based on the system of the same name described by [Bell et al.] [1] *STAR-Vote* provides a DRE-style human interface and a voter-verifiable paper trail, as well as using end-to-end cryptography to provide evidence of correct recording and tallying of votes. It supports both ballot-level risk-limiting audits and auditing by voters and other public observers.

*STAR-Vote* is comprised of multiple components: *back-end applications* that handle the generation of encryption keys, maintenance of a voter status database, maintenance of a public authenticated write-only bulletin board, and tallying of cast ballots after an election; a *polling place controller* that manages the election protocol at a given polling place; a *check-in station* used by voters to check in to a polling place; a *ballot claim station* used by voters to obtain a ballot code after checking in; a *voting terminal* at which voters mark their ballots; a *ballot reading station* at which voters can optionally verify that the computer-readable representations of their ballots match their intent; and a *vote submission station* at which voters can choose to either cast or audit/spoil their completed ballots. 

Development Process and Methodology
===

*STAR-Vote* is being developed using the Trust-by-Design (TBD) software engineering methodology.

The TBD methodology is documented in several papers published by Joe
Kiniry and his coauthors, available via http://www.kindsoftware.com/.

In general, a system is comprised of:

* a top-level readme (like this one) that includes information about
  the system's purpose, examples of its use, fundamental concepts,
  system requirements, and background literature,

* a domain analysis and a detailed architecture specifications written
  in the [Extended Business Object Notation (EBON)] [2],

* formal specifications written at the source code level in one or
  more contract-based specification languages like [Code Contracts] [3]
  (for .NET systems), the [Java Modeling Language] [4] (for JVM
  systems), or the [Executable ANSI/ISO C Specification Language
  (E-ACSL)] [5],

* protocol descriptions typically formally specified using abstract
  state machines (ASMs), petri nets, formally annotated collaboration
  diagrams, or other formal notations that have tool support for
  reasoning about such protocols,

* a hand-written set of (sub)system tests and an automatically
  generated set of unit tests (using [PEX] [6] for .NET systems and
  [JMLUnitNG] [7] for JVM ones), including reports on the completeness
  and quality of these validation artifacts, and

* a set of evidence that the system fulfills its requirements, usually
  in the form of traceable artifacts from the requirements to other
  artifacts that validate that they are satisfied (e.g., test results,
  code reviews, formal proofs, etc.).
  
Requirements
===

To be written

Current Status
===

Development of *STAR-Vote* is currently at the domain analysis and specification stage; a demonstration prototype has been constructed based on the description in the [*STAR-Vote* paper] [1] (see USAGE.md for usage instructions for the prototype).

[1]: http://www.traviscountyclerk.org/eclerk/content/images/presentations_articles/pdf_tc_elections_2013.07.26_star.pdf "STAR-Vote: A Secure, Transparent, Auditable and Reliable Voting System"

[2]: http://bon-method.com/  "The Business Object Notation"

[3]: http://research.microsoft.com/en-us/projects/contracts/  "Code Contracts library for .NET"

[4]: http://www.jmlspecs.org/  "Java Modeling Language (JML)"

[5]: http://frama-c.com/ "The Executable ANSI/ISO C Specification Language"

[6]: http://research.microsoft.com/en-us/projects/pex/  "PEX"

[7]: http://formalmethods.insttech.washington.edu/software/jmlunitng/ "JMLUnitNG"
