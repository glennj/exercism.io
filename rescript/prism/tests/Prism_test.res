open Test
open Prism

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("zero prisms", () => {
  equal(~message="zero prisms", findSequence({x: 0., y: 0., angle: 0.}, []), [])
})

test("one prism one hit", () => {
  equal(
    ~message="one prism one hit",
    findSequence({x: 0., y: 0., angle: 0.}, [{id: 1, point: {x: 10., y: 0., angle: 0.}}]),
    [1],
  )
})

test("one prism zero hits", () => {
  equal(
    ~message="one prism zero hits",
    findSequence({x: 0., y: 0., angle: 0.}, [{id: 1, point: {x: -10., y: 0., angle: 0.}}]),
    [],
  )
})

test("going up zero hits", () => {
  equal(
    ~message="going up zero hits",
    findSequence(
      {x: 0., y: 0., angle: 90.},
      [
        {id: 3, point: {x: 0., y: -10., angle: 0.}},
        {id: 1, point: {x: -10., y: 0., angle: 0.}},
        {id: 2, point: {x: 10., y: 0., angle: 0.}},
      ],
    ),
    [],
  )
})

test("going down zero hits", () => {
  equal(
    ~message="going down zero hits",
    findSequence(
      {x: 0., y: 0., angle: -90.},
      [
        {id: 1, point: {x: 10., y: 0., angle: 0.}},
        {id: 2, point: {x: 0., y: 10., angle: 0.}},
        {id: 3, point: {x: -10., y: 0., angle: 0.}},
      ],
    ),
    [],
  )
})

test("going left zero hits", () => {
  equal(
    ~message="going left zero hits",
    findSequence(
      {x: 0., y: 0., angle: 180.},
      [
        {id: 2, point: {x: 0., y: 10., angle: 0.}},
        {id: 3, point: {x: 10., y: 0., angle: 0.}},
        {id: 1, point: {x: 0., y: -10., angle: 0.}},
      ],
    ),
    [],
  )
})

test("negative angle", () => {
  equal(
    ~message="negative angle",
    findSequence(
      {x: 0., y: 0., angle: -180.},
      [
        {id: 1, point: {x: 0., y: -10., angle: 0.}},
        {id: 2, point: {x: 0., y: 10., angle: 0.}},
        {id: 3, point: {x: 10., y: 0., angle: 0.}},
      ],
    ),
    [],
  )
})

test("large angle", () => {
  equal(
    ~message="large angle",
    findSequence({x: 0., y: 0., angle: 2340.}, [{id: 1, point: {x: 10., y: 0., angle: 0.}}]),
    [],
  )
})

test("upward refraction two hits", () => {
  equal(
    ~message="upward refraction two hits",
    findSequence(
      {x: 0., y: 0., angle: 0.},
      [{id: 1, point: {x: 10., y: 10., angle: 0.}}, {id: 2, point: {x: 10., y: 0., angle: 90.}}],
    ),
    [2, 1],
  )
})

test("downward refraction two hits", () => {
  equal(
    ~message="downward refraction two hits",
    findSequence(
      {x: 0., y: 0., angle: 0.},
      [{id: 1, point: {x: 10., y: 0., angle: -90.}}, {id: 2, point: {x: 10., y: -10., angle: 0.}}],
    ),
    [1, 2],
  )
})

test("same prism twice", () => {
  equal(
    ~message="same prism twice",
    findSequence(
      {x: 0., y: 0., angle: 0.},
      [{id: 2, point: {x: 10., y: 0., angle: 0.}}, {id: 1, point: {x: 20., y: 0., angle: -180.}}],
    ),
    [2, 1, 2],
  )
})

test("simple path", () => {
  equal(
    ~message="simple path",
    findSequence(
      {x: 0., y: 0., angle: 0.},
      [
        {id: 3, point: {x: 30., y: 10., angle: 45.}},
        {id: 1, point: {x: 10., y: 10., angle: -90.}},
        {id: 2, point: {x: 10., y: 0., angle: 90.}},
        {id: 4, point: {x: 20., y: 0., angle: 0.}},
      ],
    ),
    [2, 1, 3],
  )
})

test("multiple prisms floating point precision", () => {
  equal(
    ~message="multiple prisms floating point precision",
    findSequence(
      {x: 0., y: 0., angle: -6.429},
      [
        {id: 26, point: {x: 5.8, y: 73.4, angle: 6.555}},
        {id: 24, point: {x: 36.2, y: 65.2, angle: -0.304}},
        {id: 20, point: {x: 20.4, y: 82.8, angle: 45.17}},
        {id: 31, point: {x: -20.2, y: 48.8, angle: 30.615}},
        {id: 30, point: {x: 24., y: 0.6, angle: 28.771}},
        {id: 29, point: {x: 31.4, y: 79.4, angle: 61.327}},
        {id: 28, point: {x: 36.4, y: 31.4, angle: -18.157}},
        {id: 22, point: {x: 47., y: 57.8, angle: 54.745}},
        {id: 38, point: {x: 36.4, y: 79.2, angle: 49.05}},
        {id: 10, point: {x: 37.8, y: 55.2, angle: 11.978}},
        {id: 18, point: {x: -26., y: 42.6, angle: 22.661}},
        {id: 25, point: {x: 38.8, y: 76.2, angle: 51.958}},
        {id: 2, point: {x: 0., y: 42.4, angle: -21.817}},
        {id: 35, point: {x: 21.4, y: 44.8, angle: -171.579}},
        {id: 7, point: {x: 14.2, y: -1.6, angle: 19.081}},
        {id: 33, point: {x: 11.2, y: 44.4, angle: -165.941}},
        {id: 11, point: {x: 15.4, y: 82.6, angle: 66.262}},
        {id: 16, point: {x: 30.8, y: 6.6, angle: 35.852}},
        {id: 15, point: {x: -3., y: 79.2, angle: 53.782}},
        {id: 4, point: {x: 29., y: 75.4, angle: 17.016}},
        {id: 23, point: {x: 41.6, y: 59.8, angle: 70.763}},
        {id: 8, point: {x: -10., y: 15.8, angle: -9.24}},
        {id: 13, point: {x: 48.6, y: 51.8, angle: 45.812}},
        {id: 1, point: {x: 13.2, y: 77., angle: 17.937}},
        {id: 34, point: {x: -8.8, y: 36.8, angle: -4.199}},
        {id: 21, point: {x: 24.4, y: 75.8, angle: 20.783}},
        {id: 17, point: {x: -4.4, y: 74.6, angle: 24.709}},
        {id: 9, point: {x: 30.8, y: 41.8, angle: -165.413}},
        {id: 32, point: {x: 4.2, y: 78.6, angle: 40.892}},
        {id: 37, point: {x: -15.8, y: 47., angle: 33.29}},
        {id: 6, point: {x: 1., y: 80.6, angle: 51.295}},
        {id: 36, point: {x: -27., y: 47.8, angle: 92.52}},
        {id: 14, point: {x: -2., y: 34.4, angle: -52.001}},
        {id: 5, point: {x: 23.2, y: 80.2, angle: 31.866}},
        {id: 27, point: {x: -5.6, y: 32.8, angle: -75.303}},
        {id: 12, point: {x: -1., y: 0.2, angle: 0.}},
        {id: 3, point: {x: -6.6, y: 3.2, angle: 46.72}},
        {id: 19, point: {x: -13.8, y: 24.2, angle: -9.205}},
      ],
    ),
    [
      7,
      30,
      16,
      28,
      13,
      22,
      23,
      10,
      9,
      24,
      25,
      38,
      29,
      4,
      35,
      21,
      5,
      20,
      11,
      1,
      33,
      26,
      32,
      6,
      15,
      17,
      2,
      14,
      27,
      34,
      37,
      31,
      36,
      18,
      19,
      8,
      3,
      12,
    ],
  )
})

test("complex path with multiple prisms floating point precision", () => {
  equal(
    ~message="complex path with multiple prisms floating point precision",
    findSequence(
      {x: 0., y: 0., angle: 0.},
      [
        {id: 46, point: {x: 37.4, y: 20.6, angle: -88.332}},
        {id: 72, point: {x: -24.2, y: 23.4, angle: -90.774}},
        {id: 25, point: {x: 78.6, y: 7.8, angle: 98.562}},
        {id: 60, point: {x: -58.8, y: 31.6, angle: 115.56}},
        {id: 22, point: {x: 75.2, y: 28., angle: 63.515}},
        {id: 2, point: {x: 89.8, y: 27.8, angle: 91.176}},
        {id: 23, point: {x: 9.8, y: 30.8, angle: 30.829}},
        {id: 69, point: {x: 22.8, y: 20.6, angle: -88.315}},
        {id: 44, point: {x: -0.8, y: 15.6, angle: -116.565}},
        {id: 36, point: {x: -24.2, y: 8.2, angle: -90.}},
        {id: 53, point: {x: -1.2, y: 0., angle: 0.}},
        {id: 52, point: {x: 14.2, y: 24., angle: -143.896}},
        {id: 5, point: {x: -65.2, y: 21.6, angle: 93.128}},
        {id: 66, point: {x: 5.4, y: 15.6, angle: 31.608}},
        {id: 51, point: {x: -72.6, y: 21., angle: -100.976}},
        {id: 65, point: {x: 48., y: 10.2, angle: 87.455}},
        {id: 21, point: {x: -41.8, y: 0., angle: 68.352}},
        {id: 18, point: {x: -46.2, y: 19.2, angle: -128.362}},
        {id: 10, point: {x: 74.4, y: 0.4, angle: 90.939}},
        {id: 15, point: {x: 67.6, y: 0.4, angle: 84.958}},
        {id: 35, point: {x: 14.8, y: -0.4, angle: 89.176}},
        {id: 1, point: {x: 83., y: 0.2, angle: 89.105}},
        {id: 68, point: {x: 14.6, y: 28., angle: -29.867}},
        {id: 67, point: {x: 79.8, y: 18.6, angle: -136.643}},
        {id: 38, point: {x: 53., y: 14.6, angle: -90.848}},
        {id: 31, point: {x: -58., y: 6.6, angle: -61.837}},
        {id: 74, point: {x: -30.8, y: 0.4, angle: 85.966}},
        {id: 48, point: {x: -4.6, y: 10., angle: -161.222}},
        {id: 12, point: {x: 59., y: 5., angle: -91.164}},
        {id: 33, point: {x: -16.4, y: 18.4, angle: 90.734}},
        {id: 4, point: {x: 82.6, y: 27.6, angle: 71.127}},
        {id: 75, point: {x: -10.2, y: 30.6, angle: -1.108}},
        {id: 28, point: {x: 38., y: 0., angle: 86.863}},
        {id: 11, point: {x: 64.4, y: -0.2, angle: 92.353}},
        {id: 9, point: {x: -51.4, y: 31.6, angle: 67.249}},
        {id: 26, point: {x: -39.8, y: 30.8, angle: 61.113}},
        {id: 30, point: {x: -34.2, y: 0.6, angle: 111.33}},
        {id: 56, point: {x: -51., y: 0.2, angle: 70.445}},
        {id: 41, point: {x: -12., y: 0., angle: 91.219}},
        {id: 24, point: {x: 63.8, y: 14.4, angle: 86.586}},
        {id: 70, point: {x: -72.8, y: 13.4, angle: -87.238}},
        {id: 3, point: {x: 22.4, y: 7., angle: -91.685}},
        {id: 13, point: {x: 34.4, y: 7., angle: 90.}},
        {id: 16, point: {x: -47.4, y: 11.4, angle: -136.02}},
        {id: 6, point: {x: 90., y: 0.2, angle: 90.415}},
        {id: 54, point: {x: 44., y: 27.8, angle: 85.969}},
        {id: 32, point: {x: -9., y: 0., angle: 91.615}},
        {id: 8, point: {x: -31.6, y: 30.8, angle: 0.535}},
        {id: 39, point: {x: -12., y: 8.2, angle: 90.}},
        {id: 14, point: {x: -79.6, y: 32.4, angle: 92.342}},
        {id: 42, point: {x: 65.8, y: 20.8, angle: -85.867}},
        {id: 40, point: {x: -65., y: 14., angle: 87.109}},
        {id: 45, point: {x: 10.6, y: 18.8, angle: 23.697}},
        {id: 71, point: {x: -24.2, y: 18.6, angle: -88.531}},
        {id: 7, point: {x: -72.6, y: 6.4, angle: -89.148}},
        {id: 62, point: {x: -32., y: 24.8, angle: -140.8}},
        {id: 49, point: {x: 34.4, y: -0.2, angle: 89.415}},
        {id: 63, point: {x: 74.2, y: 12.6, angle: -138.429}},
        {id: 59, point: {x: 82.8, y: 13., angle: -140.177}},
        {id: 34, point: {x: -9.4, y: 23.2, angle: -88.238}},
        {id: 76, point: {x: -57.6, y: 0., angle: 1.2}},
        {id: 43, point: {x: 7., y: 0., angle: 116.565}},
        {id: 20, point: {x: 45.8, y: -0.2, angle: 1.469}},
        {id: 37, point: {x: -16.6, y: 13.2, angle: 84.785}},
        {id: 58, point: {x: -79., y: -0.2, angle: 89.481}},
        {id: 50, point: {x: -24.2, y: 12.8, angle: -86.987}},
        {id: 64, point: {x: 59.2, y: 10.2, angle: -92.203}},
        {id: 61, point: {x: -72., y: 26.4, angle: -83.66}},
        {id: 47, point: {x: 45.4, y: 5.8, angle: -82.992}},
        {id: 17, point: {x: -52.2, y: 17.8, angle: -52.938}},
        {id: 57, point: {x: -61.8, y: 32., angle: 84.627}},
        {id: 29, point: {x: 47.2, y: 28.2, angle: 92.954}},
        {id: 27, point: {x: -4.6, y: 0.2, angle: 87.397}},
        {id: 55, point: {x: -61.4, y: 26.4, angle: 94.086}},
        {id: 73, point: {x: -40.4, y: 13.4, angle: -62.229}},
        {id: 19, point: {x: 53.2, y: 20.6, angle: -87.181}},
      ],
    ),
    [
      43,
      44,
      66,
      45,
      52,
      35,
      49,
      13,
      3,
      69,
      46,
      28,
      20,
      11,
      24,
      38,
      19,
      42,
      15,
      10,
      63,
      25,
      59,
      1,
      6,
      2,
      4,
      67,
      22,
      29,
      65,
      64,
      12,
      47,
      54,
      68,
      23,
      75,
      8,
      26,
      18,
      9,
      60,
      17,
      31,
      7,
      70,
      40,
      5,
      51,
      61,
      55,
      57,
      14,
      58,
      76,
      56,
      16,
      21,
      30,
      73,
      62,
      74,
      41,
      39,
      36,
      50,
      37,
      33,
      71,
      72,
      34,
      32,
      27,
      48,
      53,
    ],
  )
})
