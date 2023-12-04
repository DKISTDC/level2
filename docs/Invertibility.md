Invertibility
=============

This document contains all the invertibility requirements, and exact details on how they are to be obtained
* VISP only for now for simplicity

- [ ] WAITING: Han to specify waveband logic, specifics below
- [ ] HAN: What are the exact on-disk calculations?
- [ ] HAN: AOLocked: do you mean 75% of total frames are locked, or each dataset needs 75% locked or better?
- [ ] BLOCKED (Scott?): polarimetricAccuracy is null everywhere. Wait for fix and next update. We need to pass it through to the dataset view



TELESCOPE
---------

L1 doesn't reject any data based on quality measurements. It simply provides a report on the quality so scientists can decide what to do.

| Metadata   | GraphQL | GraphQL Values | Header | Header Values |
|------------|---------|----------------|--------|---------------|
| Health     | *health* | `[*Health]` | [DSHEALTH](https://tinyurl.com/dkist-spec-214#dkist-operations-keywords) | GOOD, BAD, ILL, UNKNOWN |
| GOS Status | *gosStatus* | `[*GosStatus]` | [GOS_STAT](https://tinyurl.com/dkist-spec-214#polarization-analysis-and-calibration-keywords) | open, opening, closed, closing, undefined |
| AO Locked  | *aoLocked*  | `[*AoLocked]` | [AO_LOCK](https://tinyurl.com/dkist-spec-213#adaptive-optics-keywords) | True, False |

Criteria
- Health: ~75% of frames should be `GOOD`
- GOS: 100% of the frames should be `open`
- AO: ~75% of the frames should be locked = `True`

INSTRUMENT
----------

| Metadata | GraphQL | GraphQL Values | Header | Header Values |
|----------|---------|--------|---------------|----------------|
| ~~Health~~  |         |     |               |
| Stokes   | stokesParameters | `[I,Q,U,V]` | STOKES | I,Q,U,V |
| Stokes   | hasAllStokes | `bool` | STOKES | I,Q,U,V |

- There is no separate instrument health. Use telescope health
- All stokes parameters must be present

QUALITY
-------

| Metadata | GraphQL | GraphQL Values |  Header | Header Values |
|----------|---------|----------------|---------|---------------|
| Light Level | *lightLevel* | `Distribution` | [LIGHTLVL](https://tinyurl.com/dkist-spec-214#dkist-operations-keywords) | float |
| Polarimetric Sensitivity | *polarimetricAccuracy* | `Distribution` | [POL_SENS](https://tinyurl.com/dkist-spec-214#polarization-analysis-and-calibration-keywords) | float |
| Polarimetric Sensitivity | qualityAveragePolarimetricAccuracy | `float` | [POL_SENS](https://tinyurl.com/dkist-spec-214#polarization-analysis-and-calibration-keywords) | float |
| ~~Humidity~~ |  |  |  | |

- `Distrubtion {min, p25, median, p75, max}`
- If the scientist running the experiment has allowed it to complete, these probably aren't the best indication that the dataset is good or bad. 
- Pass both of these through so we can observe what normal values are but do not disqualify


SPATIAL
-------

| Metadata       | GraphQL | Values |
|----------------|---------|--------|
| ~~Solar X-Center~~ | | |
| ~~Solar Y-Center~~ | | |
| ~~FOV Width~~ | | | |
| ~~FOV Height~~ | | | |
| ~~FOV Height~~ | | | |
| ~~Slit Width~~ | | | |
| On Disk | boundingBox | `((x1,y1),(x2,y2))` (arcseconds, string) |
| ~~Slit Horizon Azimuth~~ |         |        |               |

- Off-disk is when they block the sun's light and measure just to the side of it, to isolate atmosphere
- `boundingBox` is in coordinates from the sun's center. Can calculate on/off by knowing how many arcseconds the sun's radius is


SPECTRAL
--------

Inversion is possible only when certain spectral lines are observed

| Band      | Min  | Center | Max  | Required ViSP |
| ----------| ---- | ------ | ---- | ------------- |
| CaII 8542 | 8535 | 8542   | 8550 | Yes
| CaII 8498 | 8493 | 8498   | 8502 | Optional
| FeI       | 6297 | 6302   | 6305 | Yes

| Metadata       | GraphQL | GraphQL Values | Header | Header Values |
| -------------- | ------- | -------------- |--------| ------------- |
| Wavelength Min | wavelengthMin | `nm` |               |
| Wavelength Max | wavelengthMax | `nm` |               |
| Wave Band  |  | | [WAVEBAND](https://tinyurl.com/dkist-spec-214#dataset-keywords) | "Ca II (854.21 nm)" |
| Spec LN  |  | | [SPECLN<sl>](https://tinyurl.com/dkist-spec-214#datacenter-keywords) | |

- Wait N days for datasets to become available before processing, hoping CaII 8498 becomes available
- Dataset must include the entire band (min and max) to be counted
- [ ] HAN will confirm the min/max ranges of these bands

[spec_214]: https://tinyurl.com/dkist-spec-214
