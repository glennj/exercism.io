/* eslint-disable  space-in-parens */

const codeBase = 64;

const Diamond = () => ({
  makeDiamond: (ch) => {
    const n = ch.charCodeAt(0) - codeBase;
    let rows = [];
    for (let i = 1; i <= n; i += 1) {
      const a = new Array(n).fill(' ');
      a[i - 1] = String.fromCharCode(codeBase + i);
      rows.push( [...a].reverse().concat( a.slice(1) ).join('') );
    }
    rows = rows.concat( [...rows].reverse().slice(1) );
    return rows.map(s => `${s}\n`).join('');
  },
});

module.exports = Diamond;
