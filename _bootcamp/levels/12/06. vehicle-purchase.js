export const needsLicense = (kind) => ["car", "truck"].includes(kind);

export const chooseVehicle = (option1, option2) =>
  `${option1 <= option2 ? option1 : option2} is clearly the better choice.`;

export const calculateResellPrice = function (originalPrice, age) {
  let value;
  if (age < 3) value = 0.8;
  else if (age < 10) value = 0.7;
  else value = 0.5;
  return originalPrice * value;
};
