ALTER TABLE datasets
  ADD COLUMN ao_locked INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN health JSON NOT NULL DEFAULT '{"good":0, "bad": 0, "ill": 0, "unknown": 0}',
  ADD COLUMN gos_status JSON NOT NULL DEFAULT '{"open":0, "opening":0, "closed": 0, "closing":0, "undefined":0}',
  ADD COLUMN polarimetric_accuracy JSON NOT NULL DEFAULT '{"min":0, "p25":0, "med": 0, "p75":0, "max":0}',
  ADD COLUMN fried_parameter JSON NOT NULL DEFAULT '{"min":0, "p25":0, "med": 0, "p75":0, "max":0}',
  ADD COLUMN light_level JSON NOT NULL DEFAULT '{"min":0, "p25":0, "med": 0, "p75":0, "max":0}';


