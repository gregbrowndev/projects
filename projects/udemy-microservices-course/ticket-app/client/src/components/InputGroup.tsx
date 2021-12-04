interface InputGroupProps {
  label?: string;
  description?: string;
  placeholder?: string;
  inputType?: "text" | "email" | "password";
  required?: boolean;
}

const InputGroup = ({
  label,
  description,
  placeholder,
  inputType = "text",
  required = false,
}: InputGroupProps) => {
  return (
    <div>
      {label && (
        <label
          htmlFor="inputField"
          className="block text-sm font-medium text-gray-700"
        >
          <span>{label}</span>
          {required && (
            <strong className="pl-1">
              <abbr title="required" className="no-underline text-xs">
                *
              </abbr>
            </strong>
          )}
        </label>
      )}
      {description && (
        <p className="text-xs font-light text-gray-500">{description}</p>
      )}
      <div className="mt-1 rounded-md shadow-sm">
        <input
          type={inputType}
          className="focus:ring-indigo-500 focus:border-indigo-500 flex-1 block w-full px-3 py-2 rounded-md sm:text-sm border border-gray-300"
          id={label && "inputField"}
          placeholder={placeholder}
        />
      </div>
    </div>
  );
};

export default InputGroup;
