


Fri, Sep 5
----------

- [ ] +Crash and Print should print to the console as well
- [ ] Upload from CU Boulder Research Computing. Works, but uploading is broken
  - [x] accessing CU Boulder Research works
  - [ ] can't upload! Something wrong with transfer, not getting the local path
- [x] Bug: "Hyperbole internal error" lots of places: when clicking upload, start over with new inversion. Was a bug with hyperbole redirects


Thu, Sep 4
----------
- [x] Bug: Globus Errors are crashing the server: Reproduce: kill server in the middle of transfer and restart. you get the “Identical Transfer” error
- [x] Refactored Generate
- [x] Inversion Generate Step - shows Generate ASDF even if it is actually generating FITS files, as long as the Inversion has the generated fits set. In reality the status is based on the task itself though. Reproduce by deleting a fits while on ASDF step and restarting. The view should report the fits generation process with the skips, etc


Previous
----------
What did I decide about Globus Errors? I reverted the change to switch to throw. Globus catches errors, then uses throwError, which it requires before it can be run.
* Maybe I decided it was ok, because we can create a generate Hyperbole handler
* Created that Transfer effect
