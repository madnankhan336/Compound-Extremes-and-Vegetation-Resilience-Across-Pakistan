// Selected existing NDVI grid cells. No country-wide ROI is used.
var CHIP_DEFINITIONS = [
  {
    "chip_id": "B1_1",
    "cell_id": 35261,
    "elevation_m": 189.851135253906,
    "elev_band": 1,
    "lon": 72.8753333507899,
    "lat": 31.9251460339481,
    "bbox": [
      72.8503332364329,
      31.9001459195911,
      72.9003334651469,
      31.9501461483051
    ],
    "utm_crs": "EPSG:32643",
    "selection_lc2014": 12,
    "selection_gfsad2010": 1,
    "local_ndvi": {
      "1999-04": 0.306016653776169,
      "2000-02": 0.28869166970253,
      "2000-07": 0.401149988174439,
      "2005-07": 0.37720000743866,
      "2001-01": 0.504400014877319,
      "2001-04": 0.367300003767014,
      "2001-07": 0.399199992418289,
      "2001-10": 0.393200010061264,
      "2014-01": 0.551649987697601,
      "2014-04": 0.421299993991852,
      "2014-07": 0.378800004720688,
      "2014-10": 0.503099977970123,
      "2022-01": 0.573099970817566,
      "2022-04": 0.330749988555908,
      "2022-07": 0.525200009346008,
      "2022-10": 0.401800006628037
    }
  },
  {
    "chip_id": "B1_2",
    "cell_id": 81381,
    "elevation_m": 180.984191894531,
    "elev_band": 1,
    "lon": 66.8753059050988,
    "lat": 25.1251149288315,
    "bbox": [
      66.8503057907418,
      25.1001148144745,
      66.9003060194558,
      25.1501150431886
    ],
    "utm_crs": "EPSG:32642",
    "selection_lc2014": 10,
    "selection_gfsad2010": 5,
    "local_ndvi": {
      "1999-04": 0.137216672301292,
      "2000-02": 0.188922226428986,
      "2000-07": 0.103249996900558,
      "2005-07": 0.116700001060963,
      "2001-01": 0.125949993729591,
      "2001-04": 0.106949999928474,
      "2001-07": 0.250400006771088,
      "2001-10": 0.162100002169609,
      "2014-01": 0.15064999461174,
      "2014-04": 0.137349992990494,
      "2014-07": 0.121550001204014,
      "2014-10": 0.13510000705719,
      "2022-01": 0.20974999666214,
      "2022-04": 0.143399998545647,
      "2022-07": 0.430400013923645,
      "2022-10": 0.343899995088577
    }
  },
  {
    "chip_id": "B2_1",
    "cell_id": 37589,
    "elevation_m": 508.184356689453,
    "elev_band": 2,
    "lon": 70.2753214576571,
    "lat": 31.5751444329495,
    "bbox": [
      70.2503213433001,
      31.5501443185924,
      70.3003215720142,
      31.6001445473065
    ],
    "utm_crs": "EPSG:32642",
    "selection_lc2014": 10,
    "selection_gfsad2010": 0,
    "local_ndvi": {
      "1999-04": 0.141236662864685,
      "2000-02": 0.0750925913453102,
      "2000-07": 0.130300000309944,
      "2005-07": 0.162049993872643,
      "2001-01": 0.12950000166893,
      "2001-04": 0.1199000030756,
      "2001-07": 0.140100002288818,
      "2001-10": 0.141399994492531,
      "2014-01": 0.163949996232986,
      "2014-04": 0.18860000371933,
      "2014-07": 0.153750002384186,
      "2014-10": 0.180800005793571,
      "2022-01": 0.184949994087219,
      "2022-04": 0.158199995756149,
      "2022-07": 0.257450014352798,
      "2022-10": 0.26910001039505
    }
  },
  {
    "chip_id": "B2_2",
    "cell_id": 17232,
    "elevation_m": 902.08544921875,
    "elev_band": 2,
    "lon": 72.4253312923631,
    "lat": 34.575158155795,
    "bbox": [
      72.400331178006,
      34.550158041438,
      72.4503314067201,
      34.6001582701521
    ],
    "utm_crs": "EPSG:32643",
    "selection_lc2014": 12,
    "selection_gfsad2010": 3,
    "local_ndvi": {
      "1999-04": 0.331420004367828,
      "2000-02": 0.213907688856125,
      "2000-07": 0.565100014209747,
      "2005-07": 0.501800000667572,
      "2001-01": 0.342000007629395,
      "2001-04": 0.481099992990494,
      "2001-07": 0.621399998664856,
      "2001-10": 0.296499997377396,
      "2014-01": 0.391050010919571,
      "2014-04": 0.642799973487854,
      "2014-07": 0.568300008773804,
      "2014-10": 0.456000000238419,
      "2022-01": 0.411300003528595,
      "2022-04": 0.535350024700165,
      "2022-07": 0.608049988746643,
      "2022-10": 0.399899989366531
    }
  },
  {
    "chip_id": "B3_1",
    "cell_id": 15171,
    "elevation_m": 1550.16247558594,
    "elev_band": 3,
    "lon": 71.3753264893671,
    "lat": 34.8751595280796,
    "bbox": [
      71.3503263750101,
      34.8501594137225,
      71.4003266037242,
      34.9001596424366
    ],
    "utm_crs": "EPSG:32642",
    "selection_lc2014": 12,
    "selection_gfsad2010": 1,
    "local_ndvi": {
      "1999-04": 0.306463330984116,
      "2000-02": 0.202637031674385,
      "2000-07": 0.438250005245209,
      "2005-07": 0.535650014877319,
      "2001-01": 0.32804998755455,
      "2001-04": 0.449349999427795,
      "2001-07": 0.559499979019165,
      "2001-10": 0.285899996757507,
      "2014-01": 0.349150002002716,
      "2014-04": 0.54025000333786,
      "2014-07": 0.556150019168854,
      "2014-10": 0.404300004243851,
      "2022-01": 0.302350014448166,
      "2022-04": 0.489749997854233,
      "2022-07": 0.665849983692169,
      "2022-10": 0.389999985694885
    }
  },
  {
    "chip_id": "B3_2",
    "cell_id": 57238,
    "elevation_m": 1910.46655273438,
    "elev_band": 3,
    "lon": 66.7253052189565,
    "lat": 28.6751311675321,
    "bbox": [
      66.7003051045995,
      28.650131053175,
      66.7503053333136,
      28.7001312818891
    ],
    "utm_crs": "EPSG:32642",
    "selection_lc2014": 7,
    "selection_gfsad2010": 0,
    "local_ndvi": {
      "1999-04": 0.198946669697762,
      "2000-02": 0.165966659784317,
      "2000-07": 0.123800002038479,
      "2005-07": 0.150199994444847,
      "2001-01": 0.140249997377396,
      "2001-04": 0.134299993515015,
      "2001-07": 0.138150006532669,
      "2001-10": 0.133799999952316,
      "2014-01": 0.152750000357628,
      "2014-04": 0.162249997258186,
      "2014-07": 0.139599993824959,
      "2014-10": 0.144199997186661,
      "2022-01": 0.147049993276596,
      "2022-04": 0.149000003933907,
      "2022-07": 0.146999999880791,
      "2022-10": 0.202000007033348
    }
  },
  {
    "chip_id": "B4_1",
    "cell_id": 13199,
    "elevation_m": 3175.63452148438,
    "elev_band": 4,
    "lon": 74.7753420419254,
    "lat": 35.1751609003641,
    "bbox": [
      74.7503419275684,
      35.1501607860071,
      74.8003421562825,
      35.2001610147212
    ],
    "utm_crs": "EPSG:32643",
    "selection_lc2014": 10,
    "selection_gfsad2010": 0,
    "local_ndvi": {
      "1999-04": 0.0111413793638349,
      "2000-02": -0.0112039996311069,
      "2000-07": 0.399850010871887,
      "2005-07": 0.447149991989136,
      "2001-01": null,
      "2001-04": 0.195500001311302,
      "2001-07": 0.381949990987778,
      "2001-10": 0.209600001573563,
      "2014-01": null,
      "2014-04": 0.12720000743866,
      "2014-07": 0.39614999294281,
      "2014-10": 0.248699992895126,
      "2022-01": null,
      "2022-04": 0.249449998140335,
      "2022-07": 0.417950004339218,
      "2022-10": 0.219600006937981
    }
  },
  {
    "chip_id": "B4_2",
    "cell_id": 45356,
    "elevation_m": 2650.2060546875,
    "elev_band": 4,
    "lon": 67.6253093358102,
    "lat": 30.4251391725253,
    "bbox": [
      67.6003092214532,
      30.4001390581683,
      67.6503094501672,
      30.4501392868824
    ],
    "utm_crs": "EPSG:32642",
    "selection_lc2014": 7,
    "selection_gfsad2010": 0,
    "local_ndvi": {
      "1999-04": 0.188869997859001,
      "2000-02": 0.132074072957039,
      "2000-07": 0.15854999423027,
      "2005-07": 0.188449993729591,
      "2001-01": 0.175500005483627,
      "2001-04": 0.170200005173683,
      "2001-07": 0.164649993181229,
      "2001-10": 0.160799995064735,
      "2014-01": 0.187950000166893,
      "2014-04": 0.209849998354912,
      "2014-07": 0.205699995160103,
      "2014-10": 0.1858000010252,
      "2022-01": 0.139550000429153,
      "2022-04": 0.179199993610382,
      "2022-07": 0.188700005412102,
      "2022-10": 0.213699996471405
    }
  },
  {
    "chip_id": "B5_1",
    "cell_id": 16237,
    "elevation_m": 3514.052734375,
    "elev_band": 5,
    "lon": 73.6753370102154,
    "lat": 34.7251588419373,
    "bbox": [
      73.6503368958584,
      34.7001587275802,
      73.7003371245724,
      34.7501589562943
    ],
    "utm_crs": "EPSG:32643",
    "selection_lc2014": 10,
    "selection_gfsad2010": 0,
    "local_ndvi": {
      "1999-04": 0.0480892844498158,
      "2000-02": 0.0368920005857945,
      "2000-07": 0.573099970817566,
      "2005-07": 0.615599989891052,
      "2001-01": 0.0520000010728836,
      "2001-04": 0.196349993348122,
      "2001-07": 0.67834997177124,
      "2001-10": 0.364100009202957,
      "2014-01": null,
      "2014-04": 0.0280499998480082,
      "2014-07": 0.638499975204468,
      "2014-10": 0.337300002574921,
      "2022-01": null,
      "2022-04": 0.249500006437302,
      "2022-07": 0.675549983978272,
      "2022-10": 0.404799997806549
    }
  },
  {
    "chip_id": "B5_2",
    "cell_id": 14598,
    "elevation_m": 4370.0341796875,
    "elev_band": 5,
    "lon": 76.725350961775,
    "lat": 34.9751599855077,
    "bbox": [
      76.700350847418,
      34.9501598711507,
      76.7503510761321,
      35.0001600998648
    ],
    "utm_crs": "EPSG:32643",
    "selection_lc2014": 10,
    "selection_gfsad2010": 0,
    "local_ndvi": {
      "1999-04": 0.0402423068881035,
      "2000-02": 0.0295923072844744,
      "2000-07": 0.170249998569489,
      "2005-07": 0.170800000429153,
      "2001-01": null,
      "2001-04": -0.0124000003561378,
      "2001-07": 0.18019999563694,
      "2001-10": 0.0953999981284142,
      "2014-01": null,
      "2014-04": null,
      "2014-07": 0.163499996066093,
      "2014-10": null,
      "2022-01": null,
      "2022-04": -0.009800000116229,
      "2022-07": 0.188500002026558,
      "2022-10": 0.0803999975323677
    }
  }
];

// Reviewer 1 Comment 3 - targeted chip diagnostics only.
// Paste the complete delivered file into https://code.earthengine.google.com/.
// No imagery is exported. Missing NDVI remains masked / blank in CSV.
// This is a selected-site/selected-year test, not validation of the full record.

var CFG = {
  years: [2001, 2014, 2022],
  seasonalMonths: [1, 4, 7, 10],
  sourceMonths: ['1999-04', '2000-07', '2005-07', '2014-07', '2022-07'],
  // Keep all 10 chips for the intended pilot; use 1 for an initial small test.
  maxChips: 10,
  runCovariates: true,
  runMonthlySensorChecks: true,
  runSeasonalProfiles: true,
  runSourceFingerprint: true,
  runPairedSceneChecks: true,
  maxSceneDateGapDays: 8,
  minClearObservations: 2,
  minCommonFraction: 0.20,
  minCommonPixels: 100,
  minSeasonalMonths: 9,
  minSeasonalAmplitude: 0.05,
  activeFractionOfAmplitude: 0.20,
  driveFolder: 'Adnan_R1C3_GEE_Chips',
  exportPrefix: 'R1C3_10chips_2001_2014_2022',
  tileScale: 4,
  maxPixels: 2000000
};

// CHIP_DEFINITIONS is embedded above from the existing original grid.
var CHIPS = CHIP_DEFINITIONS.slice(0, CFG.maxChips);
var SENSOR = {
  L5: {id: 'LANDSAT/LT05/C02/T1_L2', red: 'SR_B3', nir: 'SR_B4', cirrus: false},
  L7: {id: 'LANDSAT/LE07/C02/T1_L2', red: 'SR_B3', nir: 'SR_B4', cirrus: false},
  L8: {id: 'LANDSAT/LC08/C02/T1_L2', red: 'SR_B4', nir: 'SR_B5', cirrus: true},
  L9: {id: 'LANDSAT/LC09/C02/T1_L2', red: 'SR_B4', nir: 'SR_B5', cirrus: true}
};
var PAIRS = [
  {year: 2001, a: 'L5', b: 'L7'},
  {year: 2014, a: 'L7', b: 'L8'},
  {year: 2022, a: 'L8', b: 'L9'}
];

function chipGeometry(c) { return ee.Geometry.Rectangle(c.bbox, 'EPSG:4326', false); }
function chipPoint(c) { return ee.Geometry.Point([c.lon, c.lat]); }
function chipProperties(c) {
  return {chip_id: c.chip_id, cell_id: c.cell_id, elevation_m: c.elevation_m,
    elev_band: c.elev_band, lon: c.lon, lat: c.lat, utm_crs: c.utm_crs};
}
function emptyNDVI() { return ee.Image.constant(0).rename('NDVI').updateMask(ee.Image.constant(0)); }
function reduceImage(im, reducer, c, scale) {
  return im.reduceRegion({reducer: reducer, geometry: chipGeometry(c), crs: c.utm_crs,
    scale: scale, maxPixels: CFG.maxPixels, tileScale: CFG.tileScale, bestEffort: false});
}
function numericOrZero(value) {
  // Only for counts and areas; NEVER for NDVI or sensor difference estimates.
  return ee.Number(ee.Algorithms.If(ee.Algorithms.IsEqual(value, null), 0, value));
}
function areaFraction(mask, c, scale) {
  var area = ee.Image.pixelArea();
  var total = numericOrZero(reduceImage(area.rename('area'), ee.Reducer.sum(), c, scale).get('area'));
  var numerator = numericOrZero(reduceImage(area.updateMask(mask).rename('area'),
    ee.Reducer.sum(), c, scale).get('area'));
  return ee.Algorithms.If(total.gt(0), numerator.divide(total), null);
}
function notNull(value) { return ee.Number(ee.Algorithms.If(ee.Algorithms.IsEqual(value, null), 0, 1)).eq(1); }
function differenceIfObserved(candidate, local) {
  if (local === null || local === undefined) return null;
  return ee.Algorithms.If(notNull(candidate), ee.Number(candidate).subtract(local), null);
}

function landsatCollection(sensor, start, end, c) {
  var def = SENSOR[sensor];
  return ee.ImageCollection(def.id).filterBounds(chipGeometry(c)).filterDate(start, end)
    .map(function(im) {
      var qa = im.select('QA_PIXEL');
      var bad = (1 << 0) | (1 << 1) | (1 << 3) | (1 << 4) | (1 << 5);
      if (def.cirrus) bad = bad | (1 << 2);
      var clear = qa.bitwiseAnd(bad).eq(0).and(im.select('QA_RADSAT').eq(0));
      var red = im.select(def.red).multiply(0.0000275).add(-0.2);
      var nir = im.select(def.nir).multiply(0.0000275).add(-0.2);
      // expression avoids normalizedDifference's implicit negative-input mask.
      var ndvi = nir.subtract(red).divide(nir.add(red)).rename('NDVI')
        .updateMask(clear).updateMask(nir.add(red).abs().gt(1e-6));
      ndvi = ndvi.updateMask(ndvi.gte(-1).and(ndvi.lte(1)));
      return ndvi.copyProperties(im, im.propertyNames());
    });
}
function monthlySummary(ic) {
  var n = ic.size();
  var p75 = ee.Image(ee.Algorithms.If(n.gt(0),
    ic.select('NDVI').reduce(ee.Reducer.percentile([75])).rename('NDVI'), emptyNDVI()));
  var count = ee.Image(ee.Algorithms.If(n.gt(0), ic.select('NDVI').count(),
    ee.Image.constant(0).rename('NDVI'))).rename('clear_count').unmask(0);
  // Zeros in count mean no observations; NDVI is never unmasked to zero.
  return {ndvi: p75.updateMask(count.gte(CFG.minClearObservations)), count: count, size: n};
}

function sensorStatistics(ndviA, ndviB, c) {
  var aa = ndviA.rename('a'), bb = ndviB.rename('b');
  var common = aa.mask().gt(0).and(bb.mask().gt(0));
  var diff = bb.subtract(aa).rename('difference').updateMask(common);
  var data = ee.Image.cat([aa, bb, diff, diff.abs().rename('absolute_difference'),
    diff.pow(2).rename('squared_difference')]).updateMask(common);
  var means = reduceImage(data, ee.Reducer.mean(), c, 30);
  var med = reduceImage(diff, ee.Reducer.median(), c, 30);
  var nCommon = numericOrZero(reduceImage(ee.Image.constant(1).rename('n').updateMask(common),
    ee.Reducer.count(), c, 30).get('n'));
  var frac = areaFraction(common, c, 30);
  var valid = nCommon.gte(CFG.minCommonPixels).and(ee.Number(frac).gte(CFG.minCommonFraction));
  var pearson = reduceImage(ee.Image.cat([aa, bb]).updateMask(common),
    ee.Reducer.pearsonsCorrelation(), c, 30);
  var props = {};
  props.common_pixel_count = nCommon; props.common_area_fraction = frac;
  props.assessable = valid;
  props.ndvi_a = ee.Algorithms.If(valid, means.get('a'), null);
  props.ndvi_b = ee.Algorithms.If(valid, means.get('b'), null);
  props.bias_b_minus_a = ee.Algorithms.If(valid, means.get('difference'), null);
  props.median_difference = ee.Algorithms.If(valid, med.get('difference'), null);
  props.mae = ee.Algorithms.If(valid, means.get('absolute_difference'), null);
  props.rmse = ee.Algorithms.If(valid, ee.Number(means.get('squared_difference')).sqrt(), null);
  props.spatial_correlation = ee.Algorithms.If(valid, pearson.get('correlation'), null);
  return props;
}
function sensorComparison(c, pair, month) {
  var start = ee.Date.fromYMD(pair.year, month, 1), end = start.advance(1, 'month');
  var a = monthlySummary(landsatCollection(pair.a, start, end, c));
  var b = monthlySummary(landsatCollection(pair.b, start, end, c));
  var props = chipProperties(c), stats = sensorStatistics(a.ndvi, b.ndvi, c);
  Object.keys(stats).forEach(function(k) {props[k] = stats[k];});
  props.year = pair.year; props.month = month; props.date = start.format('YYYY-MM');
  props.sensor_a = pair.a; props.sensor_b = pair.b;
  props.scene_count_a = a.size; props.scene_count_b = b.size;
  props.min_clear_observations = CFG.minClearObservations;
  props.composite = 'pixelwise monthly P75, common valid 30m support';
  props.interpretation = 'Sensor plus acquisition-date/compositing differences; descriptive spatial correlation only';
  return ee.Feature(null, props);
}
function pairedSceneComparison(c, pair, month) {
  var start = ee.Date.fromYMD(pair.year, month, 1), end = start.advance(1, 'month');
  var a = landsatCollection(pair.a, start, end, c);
  var b = landsatCollection(pair.b, start, end, c);
  var condition = ee.Filter.and(
    ee.Filter.maxDifference({difference: CFG.maxSceneDateGapDays * 86400000,
      leftField: 'system:time_start', rightField: 'system:time_start'}),
    ee.Filter.equals({leftField: 'WRS_PATH', rightField: 'WRS_PATH'}),
    ee.Filter.equals({leftField: 'WRS_ROW', rightField: 'WRS_ROW'}));
  var joined = ee.ImageCollection(ee.Join.saveBest({matchKey: 'pairedScene', measureKey: 'dateGapMs'})
    .apply(a, b, condition)).map(function(im) {
      var partner = ee.Image(im.get('pairedScene'));
      var gap = ee.Number(im.get('system:time_start')).subtract(partner.get('system:time_start')).abs();
      return im.set('dateGapDays', gap.divide(86400000));
    }).sort('dateGapDays');
  var hasPair = joined.size().gt(0);
  var first = ee.Image(ee.Algorithms.If(hasPair, joined.first(), emptyNDVI()));
  var second = ee.Image(ee.Algorithms.If(hasPair, first.get('pairedScene'), emptyNDVI()));
  var props = chipProperties(c), stats = sensorStatistics(first.select('NDVI'), second.select('NDVI'), c);
  Object.keys(stats).forEach(function(k) {props[k] = stats[k];});
  props.year = pair.year; props.month = month; props.sensor_a = pair.a; props.sensor_b = pair.b;
  props.n_candidate_temporal_pairs = joined.size();
  props.scene_a_id = ee.Algorithms.If(hasPair, first.get('LANDSAT_PRODUCT_ID'), null);
  props.scene_b_id = ee.Algorithms.If(hasPair, second.get('LANDSAT_PRODUCT_ID'), null);
  props.date_a = ee.Algorithms.If(hasPair,
    ee.Date(ee.Algorithms.If(hasPair, first.get('system:time_start'), 0)).format('YYYY-MM-dd'), null);
  props.date_b = ee.Algorithms.If(hasPair,
    ee.Date(ee.Algorithms.If(hasPair, second.get('system:time_start'), 0)).format('YYYY-MM-dd'), null);
  props.date_gap_days = ee.Algorithms.If(hasPair,
    first.get('dateGapDays'), null);
  props.composite = 'Single near-time scene pair; same WRS path/row; closest timestamps within month';
  props.interpretation = 'Date matching reduces but does not eliminate phenology and acquisition-condition differences';
  return ee.Feature(null, props);
}

function covariates(c, year) {
  var start = ee.Date.fromYMD(year, 1, 1);
  var lcIC = ee.ImageCollection('MODIS/061/MCD12Q1').filterDate(start, start.advance(1, 'year'));
  var lc = ee.Image(lcIC.first()).select('LC_Type1');
  var gfsad = ee.Image('USGS/GFSAD1000_V1').select('landcover');
  var props = chipProperties(c); props.year = year;
  props.landcover_product = 'MODIS/061/MCD12Q1 LC_Type1 IGBP';
  props.landcover_mode = reduceImage(lc, ee.Reducer.mode(), c, 500).get('LC_Type1');
  props.forest_fraction = areaFraction(lc.gte(1).and(lc.lte(5)), c, 500);
  props.shrub_savanna_fraction = areaFraction(lc.gte(6).and(lc.lte(9)), c, 500);
  props.grassland_fraction = areaFraction(lc.eq(10), c, 500);
  props.wetland_fraction = areaFraction(lc.eq(11), c, 500);
  props.cropland_fraction = areaFraction(lc.eq(12), c, 500);
  props.crop_natural_mosaic_fraction = areaFraction(lc.eq(14), c, 500);
  props.snow_ice_fraction = areaFraction(lc.eq(15), c, 500);
  props.barren_fraction = areaFraction(lc.eq(16), c, 500);
  props.landcover_observed_fraction = areaFraction(lc.mask(), c, 500);
  props.irrigated_proxy_fraction = areaFraction(gfsad.eq(1).or(gfsad.eq(2)), c, 1000);
  props.rainfed_proxy_fraction = areaFraction(gfsad.gte(3).and(gfsad.lte(5)), c, 1000);
  props.irrigation_product = 'USGS/GFSAD1000_V1'; props.irrigation_reference_year = 2010;
  props.irrigation_interpretation = 'Static 2010 cropland/irrigation proxy, not annual irrigation history';
  props.vegetation_type_interpretation = 'Broad land-cover proxy, not field-measured species or crop type';
  return ee.Feature(chipGeometry(c), props);
}

function modisPhenologyCollection(start, end, c) {
  return ee.ImageCollection('MODIS/061/MOD13Q1').filterDate(start, end)
    .filterBounds(chipGeometry(c)).map(function(im) {
      var ndvi = im.select('NDVI');
      var qa = im.select('DetailedQA');
      var good = im.select('SummaryQA').lte(1).and(qa.bitwiseAnd(1 << 10).eq(0))
        .and(qa.bitwiseAnd(1 << 14).eq(0)).and(qa.bitwiseAnd(1 << 15).eq(0));
      return ndvi.updateMask(good).updateMask(ndvi.gte(-2000).and(ndvi.lte(10000)))
        .multiply(0.0001).rename('NDVI').copyProperties(im, ['system:time_start']);
    });
}
function seasonalFeatures(c, year) {
  var rows = [];
  for (var m = 1; m <= 12; m++) {
    var start = ee.Date.fromYMD(year, m, 1), end = start.advance(1, 'month');
    var ic = modisPhenologyCollection(start, end, c);
    var monthly = ee.Image(ee.Algorithms.If(ic.size().gt(0), ic.mean(), emptyNDVI())).rename('NDVI');
    var count = ee.Image(ee.Algorithms.If(ic.size().gt(0), ic.count(),
      ee.Image.constant(0).rename('NDVI'))).rename('count').unmask(0);
    var stats = reduceImage(monthly, ee.Reducer.mean(), c, 250);
    var props = chipProperties(c); props.year = year; props.month = m;
    props.date = start.format('YYYY-MM'); props.modis_ndvi = stats.get('NDVI');
    props.valid_area_fraction = areaFraction(monthly.mask(), c, 250);
    props.mean_observation_count = reduceImage(count, ee.Reducer.mean(), c, 250).get('count');
    props.period_note = '16-day composites assigned by start date; not daily phenological dates';
    rows.push(ee.Feature(null, props));
  }
  var fc = ee.FeatureCollection(rows);
  var observed = fc.filter(ee.Filter.notNull(['modis_ndvi']));
  var n = observed.size();
  var lo = numericOrZero(observed.aggregate_min('modis_ndvi'));
  var hi = numericOrZero(observed.aggregate_max('modis_ndvi'));
  var amplitude = hi.subtract(lo);
  var adequate = n.gte(CFG.minSeasonalMonths).and(amplitude.gte(CFG.minSeasonalAmplitude));
  var threshold = lo.add(amplitude.multiply(CFG.activeFractionOfAmplitude));
  return fc.map(function(f) {
    var value = f.get('modis_ndvi');
    var active = ee.Algorithms.If(adequate.and(notNull(value)), ee.Number(value).gt(threshold), null);
    return f.set({n_observed_months: n, seasonal_profile_assessable: adequate,
      seasonal_amplitude: ee.Algorithms.If(n.gt(0), amplitude, null),
      active_greenness_proxy: active,
      activity_definition: 'Observed monthly NDVI > annual minimum + 20% amplitude; only if >=9 observed months and amplitude >=0.05'});
  });
}

function simpleMonthly(ic, aggregation) {
  var im;
  if (aggregation === 'p75') im = ic.reduce(ee.Reducer.percentile([75])).rename('NDVI');
  else if (aggregation === 'max') im = ic.max().rename('NDVI');
  else im = ic.mean().rename('NDVI');
  return ee.Image(ee.Algorithms.If(ic.size().gt(0), im, emptyNDVI()));
}
function sourceCandidates(c, month) {
  var start = ee.Date(month + '-01'), end = start.advance(1, 'month');
  var eightday = ee.ImageCollection('LANDSAT/COMPOSITES/C02/T1_L2_8DAY_NDVI')
    .filterBounds(chipGeometry(c)).filterDate(start, end).select('NDVI');
  var merged = landsatCollection('L5', start, end, c).merge(landsatCollection('L7', start, end, c))
    .merge(landsatCollection('L8', start, end, c)).merge(landsatCollection('L9', start, end, c));
  var candidates = [
    {name: 'Landsat_8day_monthly_MAX_point', ic: eightday, agg: 'max', factor: 1, scale: 30, method: 'point'},
    {name: 'Landsat_8day_monthly_MAX_chip_mean', ic: eightday, agg: 'max', factor: 1, scale: 30, method: 'chip'},
    {name: 'Landsat_L2_monthly_P75_chip_mean', ic: merged.select('NDVI'), agg: 'p75', factor: 1, scale: 30, method: 'chip'}
  ];
  ['MOD13Q1', 'MOD13A1', 'MOD13A3'].forEach(function(name) {
    var ic = ee.ImageCollection('MODIS/061/' + name).filterBounds(chipGeometry(c)).filterDate(start, end)
      .select('NDVI').map(function(im) {return im.updateMask(im.gte(-2000).and(im.lte(10000)));});
    candidates.push({name: name + '_native_monthly_mean', ic: ic, agg: 'mean', factor: 0.0001,
      scale: name === 'MOD13Q1' ? 250 : (name === 'MOD13A1' ? 500 : 1000), method: 'point'});
  });
  var rows = [];
  candidates.forEach(function(candidate) {
    var im = simpleMonthly(candidate.ic, candidate.agg).multiply(candidate.factor).rename('NDVI');
    var nativeProjection = ee.Projection(ee.Algorithms.If(candidate.ic.size().gt(0),
      ee.Image(candidate.ic.first()).select('NDVI').projection(), ee.Projection('EPSG:4326')));
    im = im.setDefaultProjection(nativeProjection);
    var value = candidate.method === 'point' ? im.reduceRegion({reducer: ee.Reducer.first(),
      geometry: chipPoint(c), crs: nativeProjection, scale: candidate.scale,
      maxPixels: CFG.maxPixels}).get('NDVI') : reduceImage(im, ee.Reducer.mean(), c, candidate.scale).get('NDVI');
    var local = c.local_ndvi[month];
    var props = chipProperties(c); props.date = month; props.candidate = candidate.name;
    props.candidate_image_count = candidate.ic.size(); props.candidate_ndvi = value;
    props.local_ndvi = local; props.difference_candidate_minus_local = differenceIfObserved(value, local);
    props.comparison_support = candidate.method; props.nominal_candidate_scale_m = candidate.scale;
    props.interpretation = 'Fingerprint candidate only; correlation or small errors alone do not prove product identity';
    rows.push(ee.Feature(null, props));
  });
  return rows;
}

function exportCSV(collection, suffix) {
  Export.table.toDrive({collection: collection, description: CFG.exportPrefix + '_' + suffix,
    folder: CFG.driveFolder, fileNamePrefix: CFG.exportPrefix + '_' + suffix, fileFormat: 'CSV'});
  // Keep previews small: run Tasks for full CSV results rather than printing all reductions.
  print(suffix + ' preview (first two rows)', collection.limit(2));
}

var chipFC = ee.FeatureCollection(CHIPS.map(function(c) {return ee.Feature(chipGeometry(c), chipProperties(c));}));
Map.centerObject(chipFC, 5);
Map.addLayer(chipFC, {color: 'yellow'}, '10 existing-grid chips');
print('Study design', {chips: CHIPS.length, overlapYears: CFG.years, comparisonMonths: CFG.seasonalMonths,
  minClearObservations: CFG.minClearObservations, exportFolder: CFG.driveFolder});
print('Scope', 'Selected chips and years only. No whole-country imagery or full 1985-2025 export.');
print('Missing values', 'Blank CSV values mean unavailable/unassessable. Do not replace them by zero. Count=0 is an observation count only.');

if (CFG.runCovariates) {
  var covRows = [];
  CHIPS.forEach(function(c) {CFG.years.forEach(function(y) {covRows.push(covariates(c, y));});});
  exportCSV(ee.FeatureCollection(covRows), '01_covariates');
}
if (CFG.runMonthlySensorChecks) {
  var sensorRows = [];
  CHIPS.forEach(function(c) {PAIRS.forEach(function(p) {
    CFG.seasonalMonths.forEach(function(m) {sensorRows.push(sensorComparison(c, p, m));});
  });});
  exportCSV(ee.FeatureCollection(sensorRows), '02_sensor_P75_checks');
}
if (CFG.runSeasonalProfiles) {
  var seasonalCollections = [];
  CHIPS.forEach(function(c) {CFG.years.forEach(function(y) {seasonalCollections.push(seasonalFeatures(c, y));});});
  exportCSV(ee.FeatureCollection(seasonalCollections).flatten(), '03_seasonal_profiles');
}
if (CFG.runSourceFingerprint) {
  var sourceRows = [];
  CHIPS.forEach(function(c) {CFG.sourceMonths.forEach(function(m) {sourceRows = sourceRows.concat(sourceCandidates(c, m));});});
  exportCSV(ee.FeatureCollection(sourceRows), '04_source_fingerprint');
}
if (CFG.runPairedSceneChecks) {
  var pairRows = [];
  CHIPS.forEach(function(c) {PAIRS.forEach(function(p) {
    CFG.seasonalMonths.forEach(function(m) {pairRows.push(pairedSceneComparison(c, p, m));});
  });});
  exportCSV(ee.FeatureCollection(pairRows), '05_near_time_scene_pairs');
}

// Interpretation boundaries:
// 1. Monthly P75 comparisons include date/phenology differences; they are not
//    pure instrument calibration or retrospective validation of every year.
// 2. Hundreds of 30m pixels in one chip are not independent replicates.
// 3. Land-cover classes proxy broad vegetation; irrigation is a 2010 snapshot.
// 4. The seasonal index is an observed-greenness proxy, not a validated crop calendar.
// 5. These covariates must be joined by cell_id/year to the existing R event records
//    before making adjusted resistance/recovery or elevation-attribution claims.
// 6. Do not apply a harmonization equation to the existing stack until its actual
//    sources, aggregation and demonstrated sensor differences are established.
