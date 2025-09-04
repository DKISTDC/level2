
Thu, Sep 4
----------
- [ ] Bug: Globus Errors are crashing the server: Reproduce: kill server in the middle of transfer and restart. you get the “Identical Transfer” error
- [x] Refactored Generate


Previous
----------
What did I decide about Globus Errors? I reverted the change to switch to throw. Globus catches errors, then uses throwError, which it requires before it can be run.
* Maybe I decided it was ok, because we can create a generate Hyperbole handler
* Created that Transfer effect
