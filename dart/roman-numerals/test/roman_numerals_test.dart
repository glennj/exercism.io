import 'package:roman_numerals/roman_numerals.dart';
import 'package:test/test.dart';

void main() {
  test("converts 1", () {
    expect(1.toRoman(), equals("I"));
  }, skip: false);

  test("converts 2", () {
    expect(2.toRoman(), equals("II"));
  }, skip: false);

  test("converts 3", () {
    expect(3.toRoman(), equals("III"));
  }, skip: false);

  test("converts 4", () {
    expect(4.toRoman(), equals("IV"));
  }, skip: false);

  test("converts 5", () {
    expect(5.toRoman(), equals("V"));
  }, skip: false);

  test("converts 6", () {
    expect(6.toRoman(), equals("VI"));
  }, skip: false);

  test("converts 9", () {
    expect(9.toRoman(), equals("IX"));
  }, skip: false);

  test("converts 16", () {
    expect(16.toRoman(), equals("XVI"));
  }, skip: false);

  test("converts 27", () {
    expect(27.toRoman(), equals("XXVII"));
  }, skip: false);

  test("converts 48", () {
    expect(48.toRoman(), equals("XLVIII"));
  }, skip: false);

  test("converts 49", () {
    expect(49.toRoman(), equals("XLIX"));
  }, skip: false);

  test("converts 59", () {
    expect(59.toRoman(), equals("LIX"));
  }, skip: false);

  test("converts 66", () {
    expect(66.toRoman(), equals("LXVI"));
  }, skip: false);

  test("converts 93", () {
    expect(93.toRoman(), equals("XCIII"));
  }, skip: false);

  test("converts 141", () {
    expect(141.toRoman(), equals("CXLI"));
  }, skip: false);

  test("converts 163", () {
    expect(163.toRoman(), equals("CLXIII"));
  }, skip: false);

  test("converts 166", () {
    expect(166.toRoman(), equals("CLXVI"));
  }, skip: false);

  test("converts 402", () {
    expect(402.toRoman(), equals("CDII"));
  }, skip: false);

  test("converts 575", () {
    expect(575.toRoman(), equals("DLXXV"));
  }, skip: false);

  test("converts 666", () {
    expect(666.toRoman(), equals("DCLXVI"));
  }, skip: false);

  test("converts 911", () {
    expect(911.toRoman(), equals("CMXI"));
  }, skip: false);

  test("converts 1666", () {
    expect(1666.toRoman(), equals("MDCLXVI"));
  }, skip: false);

  test("converts 1024", () {
    expect(1024.toRoman(), equals("MXXIV"));
  }, skip: false);

  test("converts 3000", () {
    expect(3000.toRoman(), equals("MMM"));
  }, skip: false);

  test("converts 3001", () {
    expect(3001.toRoman(), equals("MMMI"));
  }, skip: false);

  test("converts 3999", () {
    expect(3999.toRoman(), equals("MMMCMXCIX"));
  }, skip: false);
}
