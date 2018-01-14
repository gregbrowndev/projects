const validate = (val, rules, connectedValue) => {
  let isValid = true;
  for (let rule in rules) {
    if (!rules.hasOwnProperty(rule)) {
      continue;
    }
    switch (rule) {
      case "isEmail":
        isValid = isValid && emailValidator(val);
        break;
        case "notEmpty":
        isValid = isValid && notEmptyValidator(val);
        break;
      case "minLength":
        isValid = isValid && minLengthValidator(val, rules[rule]);
        break;
      case "equalTo":
        isValid = isValid && equalTo(val, connectedValue[rule]);
        break;
      default:
        isValid = true;
    }
  }
  return isValid;
};

const emailValidator = val => {
  return /[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/
    .test(
      val
    );
};

const minLengthValidator = (val, minLength) => {
  return val.length >= minLength;
};

const equalTo = (val, checkVal) => {
  return val === checkVal;
};

const notEmptyValidator = (val) => {
  return val && (val.trim() !== "");
};

export default validate;