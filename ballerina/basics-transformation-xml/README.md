# XML Fuel Allowance

Welcome to XML Fuel Allowance on Exercism's Ballerina Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

## Problem statement

Every employee of ABC Corp receives an unlimited fuel allowance. Employees are required to send a record every time they fill up their vehicles. These records are appended to the same XML file and processed at the month's end.

As a member of the digital operations team, your task is to read this XML file and write the results to another XML file. The output file should contain an entry for each employee with the following details:

You are given two `string` arguments.

1. Input XML file path.
1. Output XML file path.

Input XML file content will have the following XML schema.

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" 
    targetNamespace="http://www.so2w.org" 
    xmlns:tns="http://www.so2w.org" 
    elementFormDefault="qualified">

    <xs:element name="FuelEvents">
        <xs:complexType>
            <xs:sequence>
                <xs:element name="FuelEvent" type="tns:FuelUpEvent" nillable="true" minOccurs="0" maxOccurs="1000"/>
            </xs:sequence>
        </xs:complexType>
    </xs:element>

    <xs:complexType name="FuelEvent">
        <xs:sequence>
            <xs:element name="odometerReading" type="xs:integer"/>
            <xs:element name="gallons" type="xs:decimal"/>
            <xs:element name="gasPrice" type="xs:decimal"/>
        </xs:sequence>
        <xs:attribute name="employeeId" type="xs:integer" use="required"/>
    </xs:complexType>
</xs:schema>
```

Your task is to transform the XML input to the following XML format and write the content to the given path. The output file should contain an entry for each employee, sorted in ascending order by the `employeeId`.

```xml
<?xml version="1.0" encoding="UTF-8" ?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
   targetNamespace="http://www.so2w.org"
   xmlns:tns="http://www.so2w.org"
   elementFormDefault="qualified">
 
   <xs:element name="employeeFuelRecords">
       <xs:complexType>
           <xs:sequence>
               <xs:element name="employeeFuelRecord" type="tns:employeeFuelRecord" nillable="true" minOccurs="0" maxOccurs="97"/>
           </xs:sequence>
       </xs:complexType>
   </xs:element>
 
   <xs:complexType name="employeeFuelRecord">
       <xs:sequence>
           <xs:element name="gasFillUpCount" type="xs:integer"/>
           <xs:element name="totalFuelCost" type="xs:decimal"/>
           <xs:element name="totalGallons" type="xs:decimal"/>
           <xs:element name="totalMilesAccrued" type="xs:integer"/>
       </xs:sequence>
       <xs:attribute name="employeeId" type="xs:integer" use="required"/>
   </xs:complexType>
</xs:schema>
```

## Constraints

- Number of fill-up records `n` in the input XML file:  0 <= n <= 1000
- Number of employees in the company: 97
- Gas price is per gallon
- No missing fill-up records

## Example 1

- Input: `example01_input.xml`

```xml
<s:FuelEvents xmlns:s="http://www.so2w.org">
    <s:FuelEvent employeeId="2312">
        <s:odometerReading>230</s:odometerReading>
        <s:gallons>18.561</s:gallons>
        <s:gasPrice>4.56</s:gasPrice>
    </s:FuelEvent>
    <s:FuelEvent employeeId="2312">
        <s:odometerReading>500</s:odometerReading>
        <s:gallons>19.345</s:gallons>
        <s:gasPrice>4.89</s:gasPrice>
    </s:FuelEvent>
</s:FuelEvents>
```

- Output: `example01_output.xml`

```xml
<s:employeeFuelRecords xmlns:s="http://www.so2w.org">
    <s:employeeFuelRecord employeeId="2312">
        <s:gasFillUpCount>2</s:gasFillUpCount>
        <s:totalFuelCost>179.23521</s:totalFuelCost>
        <s:totalGallons>37.906</s:totalGallons>
        <s:totalMilesAccrued>270</s:totalMilesAccrued>
    </s:employeeFuelRecord>
</s:employeeFuelRecords>
```

## Example 2

- Input: `example02_input.xml`

```xml
<s:FuelEvents xmlns:s="http://www.so2w.org">
    <s:FuelEvent employeeId="2413">
        <s:odometerReading>4089</s:odometerReading>
        <s:gallons>21.682</s:gallons>
        <s:gasPrice>3.46</s:gasPrice>
    </s:FuelEvent>
    <s:FuelEvent employeeId="3423">
        <s:odometerReading>6582</s:odometerReading>
        <s:gallons>15.248</s:gallons>
        <s:gasPrice>4.56</s:gasPrice>
    </s:FuelEvent>
    <s:FuelEvent employeeId="2413">
        <s:odometerReading>4127</s:odometerReading>
        <s:gallons>4.221</s:gallons>
        <s:gasPrice>3.40</s:gasPrice>
    </s:FuelEvent>
    <s:FuelEvent employeeId="2413">
        <s:odometerReading>4349</s:odometerReading>
        <s:gallons>11.192</s:gallons>
        <s:gasPrice>4.10</s:gasPrice>
    </s:FuelEvent>
    <s:FuelEvent employeeId="3423">
        <s:odometerReading>6767</s:odometerReading>
        <s:gallons>8.696</s:gallons>
        <s:gasPrice>3.34</s:gasPrice>
    </s:FuelEvent>
    <s:FuelEvent employeeId="2413">
        <s:odometerReading>4547</s:odometerReading>
        <s:gallons>9.197</s:gallons>
        <s:gasPrice>2.90</s:gasPrice>
    </s:FuelEvent>
</s:FuelEvents>
```

- Output: `example02_output.xml`

```xml
<s:employeeFuelRecords xmlns:s="http://www.so2w.org">
    <s:employeeFuelRecord employeeId="2413">
        <s:gasFillUpCount>4</s:gasFillUpCount>
        <s:totalFuelCost>161.92962</s:totalFuelCost>
        <s:totalGallons>46.292</s:totalGallons>
        <s:totalMilesAccrued>458</s:totalMilesAccrued>
    </s:employeeFuelRecord>
    <s:employeeFuelRecord employeeId="3423">
        <s:gasFillUpCount>2</s:gasFillUpCount>
        <s:totalFuelCost>98.57552</s:totalFuelCost>
        <s:totalGallons>23.944</s:totalGallons>
        <s:totalMilesAccrued>185</s:totalMilesAccrued>
    </s:employeeFuelRecord>
</s:employeeFuelRecords>
```

## Hints

- Use the [`ballerina/io` module](https://lib.ballerina.io/ballerina/io/latest) to read/write XML files.
- These [XML templates](https://ballerina.io/learn/by-example/xml-templates) can be used to create XML values.
- The [XML data model](https://ballerina.io/learn/by-example/xml-data-model)
- [XML operations](https://ballerina.io/learn/by-example/xml-operations)
- [XML subtypes](https://ballerina.io/learn/by-example/xml-subtyping)
- Use [XML navigation expressions](https://ballerina.io/learn/by-example/xml-navigation) to extract data from the XML payload.
- [Combining XML templates and queries](https://ballerina.io/learn/by-example/xml-templates-and-query)
- [XML namespaces](https://ballerina.io/learn/by-example/xml-namespaces) and [XML namespace declarations](https://ballerina.io/learn/by-example/xmlns-declarations)
- [Ballerina table syntax](https://ballerina.io/learn/by-example/table)

## Source

### Created by

- @vordimous

### Contributed to by

- @vordimous

### Based on

This is an exercise to introduce users to converting the XML values to application-specific types to work with data in a type-safe manner using Ballerina. - https://ballerina.io/learn/by-example/xml-templates