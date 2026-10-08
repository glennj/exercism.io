type point = {x: float, y: float, angle: float}
type prism = {id: int, point: point}

let findSequence: (point, array<prism>) => array<int>
