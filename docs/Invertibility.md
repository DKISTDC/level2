Invertibility
=============

This document contains all the invertibility requirements, and exact details on how they are to be obtained
* VISP only for now for simplicity

TODO: Update Metadata API
-------------------------
The following headers need to be promoted to Metadata GraphQL API

- [ ] Implement `health`
- [ ] Implement `gosStatus`
- [ ] Implement `aoLock`
- [ ] Implement `lightLevel`

TELESCOPE
---------

L1 doesn't reject any data based on quality measurements. It simply provides a report on the quality so scientists can decide what to do.

| Metadata   | GraphQL | GraphQL Values | Header | Header Values |
|------------|---------|----------------|--------|---------------|
| Health     | *health* | `[*Health]` | [DSHEALTH](https://tinyurl.com/dkist-spec-214#dkist-operations-keywords) | GOOD, BAD, ILL, UNKNOWN |
| GOS Status | *gosStatus* | `[*GosStatus]` | [GOS_STAT](https://tinyurl.com/dkist-spec-214#polarization-analysis-and-calibration-keywords) | open, opening, closed, closing, undefined |
| AO Locked  | *aoLocked*  | `[*AoLocked]` | [AO_LOCK](https://tinyurl.com/dkist-spec-213#adaptive-optics-keywords) | True, False |

- `*Value:` a set of *unique* values. If there are 30 GOOD frames and 1 ILL frame, the value of `health` will be `["GOOD","ILL"]`

Health
- Takes the worst of all the instruments to create the summary header. One per FITS file
- [ ] (Han) Criteria?

GOS Status
- [ ] (Han) Criteria?

AO Locked
- normally always locked during ViSP measurements. 
- [ ] (Han) Criteria? What if this assumption isn't true?

INSTRUMENT
----------

| Metadata | GraphQL | Header | Header Values |
| -------- | ------- | ------ | ------------- |
| ~~Health~~   |         |     |               |
| Stokes   | stokesParameters, hasAllStokes | STOKES | I,Q,U,V |

- There is no separate instrument health. Use telescope health
- [ ] (Han) Stokes - Criteria? Reject if any parameters are not accounted for?


QUALITY
-------

| Metadata | GraphQL | GraphQL Values |  Header | Header Values |
|----------|---------|----------------|---------|---------------|
| Light Level | *qualityLightlevel* | `{mean, deviation}` | [LIGHTLVL](https://tinyurl.com/dkist-spec-214#dkist-operations-keywords) | float |
| Polarimetric Sensitivity | qualityAveragePolarimetricAccuracy | (float) | [POL_SENS](https://tinyurl.com/dkist-spec-214#polarization-analysis-and-calibration-keywords) |  |
| ~~Humidity~~ |  |  |  | |

- [ ] (Han) `lightLevel` - criteria?
- [ ] (Han) `qualityAveragePolarimetricAccuracy` - criteria? Is average enough?


SPATIAL
-------

| Metadata       | GraphQL | Header | Header Values |
| -------------- | ------- | ------ | ------------- |
| ~~Solar X-Center~~ |         |        |               |
| ~~Solar Y-Center~~ |         |        |               |
| ~~FOV Width~~ |         |        |               |
| ~~FOV Height~~ |         |        |               |
| ~~FOV Height~~ |         |        |               |
| ~~Slit Width~~ |         |        |               |
| On Disk        | boundingBox (arcseconds, string) ((x1,y1),(x2,y2)) |        |               |
| ~~Slit Horizon Azimuth~~ |         |        |               |

- Off-disk is when they block the sun's light and measure just to the side of it, to isolate atmosphere
- `boundingBox` is in coordinates from the sun's center. Can calculate on/off by knowing how many arcseconds the sun's radius is
- [ ] (Han) exact criteria?



SPECTRAL
--------

Inversion is possible only when certain spectral lines are observed

| Band      | Min  | Center | Max  | Required ViSP |
| ----------| ---- | ------ | ---- | ------------- |
| CaII 8542 | 8535 | 8542   | 8550 | Yes
| CaII 8498 | 8493 | 8498   | 8502 | Optional
| FeI       | 6297 | 6302   | 6305 | Yes

| Metadata       | GraphQL | Header | Header Values |
| -------------- | ------- | ------ | ------------- |
| Wavelength Min | wavelengthMin (nm) |        |               |
| Wavelength Max | wavelengthMax (nm) |        |               |
| Wave Band  |  | [WAVEBAND](https://tinyurl.com/dkist-spec-214#dataset-keywords) | "Ca II (854.21 nm)" |
| Spec LN  |  | [SPECLN<sl>](https://tinyurl.com/dkist-spec-214#datacenter-keywords) | ? |



- WAVEBAND is newer, not present in all datasets
- SPECNLN is not available in all datasets
- Use wavelengthMin and max, more reliable
- Wait N days for datasets to become available before processing, hoping CaII 8498 becomes available
- [ ] (Han) Which overlap rules should we use? Overlapping the center? Overlapping the range at all?




[spec_214]: https://tinyurl.com/dkist-spec-214
