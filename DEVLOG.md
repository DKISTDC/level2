
- [ ] mag Inclination, Azimuth - negative values +360 degrees
- [x] Send Han my notebook, and verify QQ
- [ ] After downloading the fits files, get caught in a loop SUCCEEDED, over and over. Because it's still in the Transferring step, but it is no longer transferring. Somebody should update the status. Maybe fixed?
- [ ] Stuart: nice to see units on dataset page

## Mon, Oct 27
- [ ] BUG - Gen Worker - can't canonical before downloading
- [ ] Test frame-cataloger: docker nonsense

## Fri, Oct 24
- [x] L2 is dumping catalog.frame.m messages into rabbitmq

## Thu, Oct 23
*Han has identified a new dataset to use, somehow he's only JUST starting on it now that we are meeting today. He likes the fried parameter, has a workflow for *
- [x] Added browseMovieUrl link on datasets table, and embedded into the dataset details

## Wed, Oct 22
- [x] small fixes of histogram
- [x] deploy changes for Han, update to hyperbole 0.5
- [x] BUG: publish wasn't showing transfer progress
- [x] BUG: publish isn't really updating if it errors or completes before getting to the transfer status screen. Try using the dev publish, or throwing an error immediately

## Tue, Oct 21
- [x] attempted iron plot, realized it is tons of work! We don't have the ability to read BinTableHDUs via haskell. Could do with a python sub-process. Told Han it's not worth it

## Fri, Oct 16
> *working from the van
- [ ] Han: generate iron plot (needs example)

IRON PLOT
* button to generate
* display the image if it already exists
* track the task if it is generating (in-memory)

## Thu, Oct 16
> *working from the van, actually slept

- [x] finished view proposals, removed most of the JS, better filtering
- [x] Han: show R0 in table and histogram

## Tue, Oct 14

> *flying back from Brazil*

- [x] optimize view.proposals with js stuff

## Mon, Sep 19
- [ ] More globus testing stuff. Use a remote folder

## Fri, Sep 19
- [ ] better way to test globus paths. Relative paths not working

## Thu, Sep 18
- [x] publish button does real transfer
- [x] error in publish step displays in generate

## Wed, Sep 17
- [x] Messed around with codex
- [x] implemented new transferPublish


## Tue, Sep 6
- [ ] Refresh globus access token when it expires...

Tue, Sep 6
----------
- [ ] Refresh globus access token when it expires...
- Hyperbole documentation
- Hyperbole path refactor
- Hyperbole example links
(didn't start on the report globus task)

Mon, Sep 6
----------
- Hyperbole

Fri, Sep 5
----------

- [x] Can upload from CU Boulder Research Computing and goes through
- [x] +Crash and Print should print to the console as well
- [x] Upload from CU Boulder Research Computing. Works, but uploading is broken
- [x] Require login immediately: all application code can expect Transfer
  - [x] accessing CU Boulder Research works
  - [x] can't upload! Something wrong with transfer, not getting the local path
- [x] Bug: "Hyperbole internal error" lots of places: when clicking upload, start over with new inversion. Was a bug with hyperbole redirects
- [x] Bug: File missing exception crashes app


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
