Invertibility
=============

This document contains all the invertibility requirements, and exact details on how they are to be obtained
* VISP only for now for simplicity




SPEC-2 30
---------

VISP: 4.1.1

- [ ] (Frazer/Scott) What does this mean?

TELESCOPE
---------

L1 doesn't reject any data based on quality measurements. It simply provides a report on the quality so scientists can decide what to do.

| Metadata | GraphQL | Header | Header Values |
| -------- | ------- | ------ | ------------- |
| [Health](#telescope_health)     | _______  | [DSHEALTH](https://tinyurl.com/dkist-spec-214#dkist-operations-keywords) | GOOD, BAD, ILL, UNKNOWN |
| [GOS Status](#gos_status) | _______  | [GOS_STAT](https://tinyurl.com/dkist-spec-214#polarization-analysis-and-calibration-keywords) | open, opening, closed, closing, undefined |
| [AO Status](#ao_status) | _______  | [AO_LOCK](https://tinyurl.com/dkist-spec-213#adaptive-optics-keywords) | True, False |

Health
- Takes the worst of all the instruments to create the summary header. One per FITS file
- [ ] (Scott/Frazer) Metadata?
- [ ] (Han) What are the rules? All Good? Percent of datasets good?

GOS Status
- [ ] (Han) Criteria?

AO Status
- normally always locked during ViSP measurements. 
- [ ] (Han) Do we reject if any are not locked? Or certain percent?

INSTRUMENT
----------

| Metadata | GraphQL | Header | Header Values |
| -------- | ------- | ------ | ------------- |
| Health   |         |        |               |
| Stokes   | stokesParameters, hasAllStokes | STOKES | I,Q,U,V |

- [ ] (Han/Frazer) Instrument Health - what is this?
- [ ] (Han) Stokes - Reject if any do not exist?


QUALITY
-------

| Metadata | GraphQL | Header | Header Values |
| -------- | ------- | ------ | ------------- |
| Light Level | ? | [LIGHTLVL](https://tinyurl.com/dkist-spec-214#dkist-operations-keywords) | float |
| Polarimetric Sensitivity | qualityAveragePolarimetricAccuracy (float) | [POL_SENS](https://tinyurl.com/dkist-spec-214#polarization-analysis-and-calibration-keywords) |  |
| ~~Humidity~~ |  |  |  |

- [ ] (Scott) Light Level - Metadata?
- [ ] (Han) POL_SENS - criteria?


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
| On Disk        | boundingBox (string, arcseconds) ((x1,y1),(x2,y2)) |        |               |
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
| Wavelength Min | wavelengthMin |        |               |
| Wavelength Max | wavelengthMax |        |               |
| Wave Band  |  | [WAVEBAND](https://tinyurl.com/dkist-spec-214#dataset-keywords) | "Ca II (854.21 nm)" |
| Spec LN  |  | [SPECLN<sl>](https://tinyurl.com/dkist-spec-214#datacenter-keywords) | ? |



- WAVEBAND is newer, not present in all datasets
- SPECNLN is not available in all datasets
- Use wavelengthMin and max, more reliable
- Wait N days for datasets to become available before processing, hoping CaII 8498 becomes available
- [ ] (Scott) what are the units of wavelengthMax and wavelengthMin?
- [ ] (Han) What overlap rules should we use? Overlapping the center? Overlapping the range at all?





[spec_214]: https://docs.dkist.nso.edu/projects/data-products/en/stable/specs/spec-214.html
[spec_214]: https://tinyurl.com/dkist-spec-214
