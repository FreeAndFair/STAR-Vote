STAR-Vote
===

*STAR-Vote* is part of the Verifiable Elections software suite of products, developed by Free & Fair. It is a *S*ecure, *T*ransparent, *A*uditable and *R*eliable voting system that runs on off-the-shelf computing hardware, based on the system of the same name described by [Bell et al.] [1] *STAR-Vote* provides a DRE-style human interface and a voter-verifiable paper trail, as well as using end-to-end cryptography to provide evidence of correct recording and tallying of votes. It supports both ballot-level risk-limiting audits and auditing by voters and other public observers.

*STAR-Vote* is comprised of multiple components: *back-end applications* that handle the generation of encryption keys, maintenance of a voter status database, maintenance of a public authenticated write-only bulletin board, and tallying of cast ballots after an election; a *polling place controller* that manages the *STAR-Vote* protocol at a given polling place; a *check-in station* used by voters to check in to a polling place; a *ballot claim station* used by voters to obtain a ballot code after checking in; a *voting terminal* at which voters mark their ballots electronically and receive filled paper ballots; a *ballot reading station* that can scan a filled paper ballot and read it to the voter (for accessibility purposes); and a *vote submission station* at which voters can cast their completed ballots. 

### Voter Flow Overview

From the voter's perspective, there are several steps in the *STAR-Vote* process; these result in a voter flow very similar to that typically found in traditional polling places. Note that this is a high-level overview; many details of the process are not described here.

1. *Sign-in*. The voter signs in with a poll worker (either electronically or using traditional physical means). A sticker with the voter's name, precinct, and ballot style is generated, placed in a poll book and signed by the voter.
2. *Ballot code generation*. The voter's sticker is scanned, and a ballot code is generated. This code, which identifies the voter's ballot style but is otherwise not linked to the voter's identity in any way, is given to the voter. 
3. *Voting*. The voter approaches a voting terminal, enters his ballot code, and votes (as on any other DRE system).
4. *Printing*. The voting terminal prints two items: a paper ballot including both a human-readable summary and a computer-readable representation of the voter's selections, and a take-home receipt that the voter can later use to ensure his vote was counted.
5. *Review*. The voter, if he chooses, reads the human-readable summary  or (for accessibility) uses a ballot reading station to verify that his vote was recorded as intended.
6. *Cast or Audit/Spoil*. The voter either casts his ballot, by depositing it at the vote submission station, or audits/spoils it, by giving it to a poll worker who scans it into the controller. If the voter audits/spoils his ballot, he can return to the *Voting* step and generate another.
7. *Post-Election Review*. Once the election is over, the voter can check that each ballot was appropriately counted and/or spoiled by using the corresponding receipt from the *Printing* step.

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

* Must provide a user interface for checking in to a polling place.
* Must provide a user interface for obtaining a ballot code.
* Must provide a user interface for ballot marking.
* Must correctly generate human-readable and encrypted computer-readable representations of marked ballots.
* Must provide user interfaces for vote casting and auditing/spoiling.
* Must provide a user interface for ballot reading (i.e., scanning a printed ballot and telling the voter what it says, for accessibility purposes).
* Must correctly tally encrypted representations of marked ballots.
* Must run on off-the-shelf computing equipment.
* Must maintain voter privacy (i.e., it must be impossible for an observer to link a voter with his vote).
* Must be receipt-free (i.e., it must be impossible for a voter to prove to a third party how he voted).
* Must provide all required information for post-election risk-limiting audits. 


Secondary Requirements
===

#### Usability:

* The user interface for running the election (generating keys, tallying votes, auditing, etc.) must be easy to use for non-technical users (election officials).
* The user interfaces for all polling place components must be trivial to use for non-technical users (polling place volunteers, voters).
* The voting experience should be as similar as possible to traditional polling place voting.

#### Persistence

* The system will exhibit minimal data loss from an arbitrary failure (e.g., a typical system failure like a Windows crash) of any back-end system; in no case will any vote data be lost. 
* The system will exhibit no data loss from an arbitrary failure of any polling place system, but may in some failure modes intentionally discard data to preserve voter privacy.
  
#### Scalability:

* The polling place components must support polling places requiring thousands of ballot styles, at which tens of thousands of voters may vote in a single day. 

#### Power:

* Polling place components must be deployable on hardware that can run on batteries for at least 8 (?) continuous hours.

#### Analysis:

* The system should be able to provide information about voting activity (e.g. numbers of votes cast/audited, number of voters entering the polling place, etc.)

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
