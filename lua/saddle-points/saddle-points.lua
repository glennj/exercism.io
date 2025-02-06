return function(matrix)
  local rowMax = {}
  local colMin = {}

  for i = 1, #matrix do
    rowMax[i] = math.mininteger
    for j = 1, #matrix[i] do
      if i == 1 then 
        colMin[j] = math.maxinteger
      end
      
      if matrix[i][j] > rowMax[i] then
        rowMax[i] = matrix[i][j]
      end
      if matrix[i][j] < colMin[j] then
        colMin[j] = matrix[i][j]
      end
    end
  end

  local saddlePoints = {}
  for i = 1, #matrix do
    for j = 1, #matrix[1] do
      if matrix[i][j] == rowMax[i] and matrix[i][j] == colMin[j] then
        table.insert(saddlePoints, {row = i, column = j})
      end
    end
  end
  return saddlePoints
end
