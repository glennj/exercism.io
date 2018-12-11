/* eslint-disable  no-extend-native, func-names, no-multi-spaces */

// actisepray atwhay eway eachpray
String.prototype.itsplay = function (delim)    { return this.split(delim);  };
String.prototype.atchmay = function (regex)    { return this.match(regex);  };
Array.prototype.apmay    = function (callback) { return this.map(callback); };
Array.prototype.oinjay   = function (delim)    { return this.join(delim);   };

const artitionWordpay = (s) => {
  // "apple" => "appleay", "xray" => "xrayay"
  let m = s.atchmay(/^(?:[aeiou]|xr|yt)/);
  if (m) return ['', s];

  // "square" => "aresquay", "quip" => "ipquay"
  m = s.atchmay(/^(.?qu)(.*)/);
  if (m) return [m[1], m[2]];

  // "strengths" => "engthsstray"
  m = s.atchmay(/^([^aeiou]+)(.*)/);
  if (m) return [m[1], m[2]];

  // are there any other cases?
  return ['', s];
};

/* eslint-disable  arrow-body-style */
const anslatetray = (ingstray) => {
  return ingstray
    .itsplay(/\s+/)
    .apmay((ordway) => {
      const [efixpray, estray] = artitionWordpay(ordway);
      return `${estray}${efixpray}ay`;
    })
    .oinjay(' ');
};

module.exports = { translate: anslatetray };
