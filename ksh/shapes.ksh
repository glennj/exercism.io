#!/usr/bin/env ksh
 
typeset -T Object_t=(
   integer -S -h 'number of objects' count=0
 
   create() {
      (( _.count--))
   }
)
 
typeset -T Color_t=(
    Object_t _
    typeset -h 'fill color' fill="blue"
 
    function getcolor {
         print -r ${_.fill}
    }
 
    function setcolor {
        _.fill=$1
    }
)
 
typeset -T Shape_t=(
    Color_t _
    integer -h 'offset x' offsetx=0
    integer -h 'offset y' offsety=0
    float -h 'dimension a' a=0
)
 
typeset -T Circle_t=(
    Shape_t _
 
    create() {
        (( _.count++))
    }
 
    area() {
        print -r $(( 3.14 *_.a *_.a))
    }
 
    circumference() {
         print -r $(( 6.28 * _.a))
    }
)
 
typeset -T Rectangle_t=(
    Shape_t _
    float -h 'dimension b' b=0
 
    create() {
        (( _.count++))
    }
 
    area() {
         print -r $((_.a *_.b))
    }
 
    perimeter() {
         print -r $(( 2 * (_.a + _.b)))
    }
)
 
typeset -T Triangle_t=(
    Rectangle_t _
    float -h 'dimension c' c=0
 
    create() {
        (( _.count++))
    }
 
    area() {
         print -r $(( _.a * _.b / 2))
    }
 
    perimeter() {
         print -r $(( _.a + _.b  + _.c))
    }
)
 
echo "Creating rectangle1 with default fill and offsets ...."
Rectangle_t rectangle1=(a=2 b=4)
 
echo "Area of rectangle1 is: ${rectangle1.area}"
echo "Perimeter of rectangle1 is: ${rectangle1.perimeter}"
echo "Color of rectangle1 is: ${rectangle1.fill}"
echo "Co-ordinates of rectangle1 are: (${rectangle1.offsetx}, ${rectangle1.offsety})"
echo "Rectangle1 is of type: ${@rectangle1}"
echo "Number of objects created so far : ${.sh.type.Object_t.count}"
echo
 
echo "Creating circle1 with red fill and offset of (5,10) ...."
Circle_t    circle1=(a=2 offsetx=5 offsety=10)
circle1.fill="red"
echo "Area of circle1 is: ${circle1.area}"
echo "Circumference of circle1 is: ${circle1.circumference}"
echo "Color of circle1 is: ${circle1.fill}"
echo "Co-ordinates of circle1 are: (${circle1.offsetx}, ${circle1.offsety})"
echo
 
echo "Changing circle1 fill color to green ...."
circle1.setcolor green
echo "Color of circle1 is: ${circle1.getcolor}"
echo "Circle1 is of type: ${@circle1}"
echo "Number of objects created so far: ${.sh.type.Object_t.count}"
echo
 
echo "Creating triangle1 with offset of (12,12) ...."
.sh.type.Triangle_t=(offsetx=12 offsety=12)
Triangle_t  triangle1=(a=3 b=4 c=5)
 
echo "Area of triangle1 is: ${triangle1.area}"
echo "Perimeter of triangle1 is: ${triangle1.perimeter}"
echo "Color of triangle1 is: ${triangle1.fill}"
echo "Co-ordinates of triangle1 are: (${triangle1.offsetx}, ${triangle1.offsety})"
echo "Triangle1 is of type: ${@triangle1}"
echo "Number of objects created so far: ${.sh.type.Object_t.count}"
echo
 
echo "Creating triangle2 and triangle3 and comparing them ...."
Triangle_t triangle2=(a=6 b=3 c=5)
Triangle_t triangle3=(a=6 b=3 c=5)
if [[ $triangle2 == $triangle3 ]]
then
    echo "CORRECT: triangle2 and triangle3 match"
else
    echo "ERROR: triangle2 and triangle3 do not match"
fi
 
echo "Creating triangle4 by assignment from triangle3 and comparing them ...."
Triangle_t triangle4=triangle3
if [[ $triangle4 == $triangle3 ]]
then
    echo "CORRECT: triangle4 and triangle3 match"
else
    echo "ERROR: triangle4 and triangle3 do not match"
fi
 
echo "Changing color of triangle4 and comparing them again ...."
triangle4.fill="green"
if [[ $triangle4 != $triangle3 ]]
then
    echo "CORRECT: triangle4 and triangle3 differ"
else
    echo "ERROR: triangle4 and triangle3 do not differ"
fi
 
exit 0
