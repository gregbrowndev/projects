import { ChangeEvent } from "react";

interface InputGroupProps {
  id: string;
  label?: string;
  value?: string;
  description?: string;
  placeholder?: string;
  inputType?: "text" | "email" | "password";
  required?: boolean;
  onChange?: (e: ChangeEvent<HTMLInputElement>) => void;
}

const InputGroup = ({
  id,
  label,
  value,
  description,
  placeholder,
  inputType = "text",
  required = false,
  onChange,
}: InputGroupProps) => {
  return (
    <div>
      {label && (
        <label
          htmlFor={id}
          className="block text-base font-medium text-gray-700"
        >
          <span>{label}</span>
          {required && (
            <strong className="pl-1">
              <abbr title="required" className="no-underline text-base">
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
          value={value}
          className="focus:ring-indigo-500 focus:border-indigo-500 flex-1 block w-full px-3 py-2 rounded-md sm:text-sm border border-gray-300"
          id={label && id}
          placeholder={placeholder}
          onChange={onChange}
        />
      </div>
    </div>
  );
};

export default InputGroup;
