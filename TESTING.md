This document contains a little documentation on how I tested that the voting
terminal was cross-platform, where we got the hardware for testing (and what
the hardware was, to the best of my knowledge), and the test results.

One-time setup (done once before a batch of testing different platforms):
1. Set up the controller and terminal following the instructions in README.md.
2. Use `ip addr` to determine where the server is running.
3. Enable port 8004 in your firewall. For Fedora, this is `sudo
   firewall-config`, then add 8004/tcp in the "Ports" section of the Runtime
   config.
4. Visit `localhost:8003/generateCode` and type in "oregon-2014" to get a code
   for use below.

Per-device:
1. Note down as many details about the hardware, OS, and browser as you can.
   `javascript:alert(navigator.userAgent)` may be helpful here.
2. Connect to Locus.
3. Open a browser and open http://<ip>:8004/ in a new tab.
6. Enter the code from the one-time setup.
7. "pass A" if there are 7 choices for Oregon Governor presented, 6 of which have
   titles, together with a "next step" button.
8. "pass B" if clicking on a radio button selects it and clicking on a name
   selects it the associated radio button.
9. Select "Paul Grad (L)". Click "next step".
10. "pass C" if there are 5 choices for US Senator, 4 of which have titles,
    together with a "previous step" and "next step" buttons.
11. Select "Christina Jean Lugo (Grn)". Click the browser back button.
12. "pass D" if "Paul Grad (L)" is selected.
13. Click the browser forward button.
14. "pass E" if "Christina Jean Lugo (Grn)" is selected.
15. Click "previous step".
16. "pass F" if "Paul Grad (L)" is selected.
17. Click "next step".
18. "pass G" if "Christina Jean Lugo (Grn)" is selected.
19. Click "next step".
20. "pass H" if there are 5 choices for US Representative, 4 of which have
    titles, together with "previous step" and "next step" buttons.
21. Click "next step".
22. "pass I" if the review page has "Paul Grad (L)" for Governor, "Christina Jean
    Lugo (Grn)" for Senator, and the Representative slot is highlighted yellow
    with a strike mark where the name should go, together with "previous step"
    and "print ballot to proceed" buttons.
23. Click "print ballot to proceed" button.
24. "pass J" if you voted.
25. Close the tab.
26. If relevant, forget the Locus connection details.

Test results:

iPod Touch
Model MD717LL/A
Version 7.1.1
Safari
borrowed from Dave Archer/the PROCEED project (lightning cable borrowed from
Dan Zimmerman) [two iPods returned, cable returned]
fail A - only 5 governors have titles
    changed to pass A after commit afc27ab
pass B
pass C
pass D
pass E
pass F
pass G
fail H - only 4 representatives are visible/reachable
    changed to pass H after commit afc27ab
fail I - strike mark not visible
    changed to pass I after commit afc27ab
pass J

Note II
borrowed (with cable) from Ledah Casburn [tablet+cable returned]
Model number SGH-I317M
Android version 4.3
Baseband version I317MUMUCML2
Kernel version 3.0.31-2505907 se.infra@R0210-19 #1 Wed Feb 5 22:15:30 KST 2014
Build number JSS15JJ307MUMUCNB1
Browser is just labeled "Internet"
pass A-J

Asus Android
borrowed from Jesse Hallett [returned]
Asus Transformer Pad TF700T
Android version 4.4.2
Kernel version 3.1.10-cyanogenmod-g057aa0a build03@cyanogenmod #1
ARMv7 Processor rev 9 v7l
CyanogenMod version 11-20140104-SNAPSHOT-M2-tfg00t
Build number cm_tf700t-userdebug 4.4.2 KOT49H 87ca68fb94 test-keys
Firefox 35.0beta
pass A-J

Kindle Fire HDX 8.9 (3rd Generation)
Silk Browser
borrowed from Tristan Ravitch [returned]
pass A-J

Nexus 7 (2nd generation)
Android version 4.4.4
Kernel version 3.4.0-g03485a6 android-build@vpbs1.mtv.corp.google.com #1
Build number KTU84P
Google Chrome 34.0.1847.114
borrowed from Tom DuBuisson [returned]
pass A-J

LG VK810 4G Rev 1.0
Android 4.4.2
Kernel 3.4.0 lge@android-build
Build number KOT49I.VK81022B
Software version VK81022B
Google Chrome 39.0.2171.93
borrowed from Anne-Marie [returned]
pass A-J

iPad
borrowed from Chris Falbusch
OS Version 5.1.1 (9B206)
Model MB293LL
Safari; user agent says (among other things) Mozilla/5.0 AppleWebKit/534.46 Version/5.1 Mobile/9B206 Safari/7534.48.3
pass A
fail B - clicking names does not select a radio button
    changed to pass B after commit a6d19ac
pass C
pass D
pass E
pass F
pass G
pass H
pass I
pass J

Motorola Xoom
borrowed from Mike Seitz [returned]
Android version 4.1.2
Equipment ID IHDP56LU2
Baseband version CDMA_N_03.1A.71P
Kernel version 2.6.39.4-g4e32b94 android-build@vpbs1 ) #1 SMP PREEMPT
[mismatched parenthesis comes from the device's own report]
Build number JZO54M
Opera Mini couldn't even reach http://<ip>:8004/
Chrome 32.0.1700.99 pass A-J
something just labeled "Browser" pass A-J
