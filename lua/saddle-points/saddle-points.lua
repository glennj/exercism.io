return function(matrix)
  local rowMax = {}
  local colMin = {}

  for i = 1, #matrix do
    for j = 1, #matrix[i] do
      rowMax[i] = j == 1 and matrix[i][j] or math.max(matrix[i][j], rowMax[i])
      colMin[j] = i == 1 and matrix[i][j] or math.min(matrix[i][j], colMin[j])
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
