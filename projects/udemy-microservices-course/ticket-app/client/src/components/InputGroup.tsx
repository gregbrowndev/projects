import { ChangeEvent, FocusEvent } from "react";
import { classnames } from "tailwindcss-classnames";

export interface InputGroupProps {
  id: string;
  name?: string;
  label?: string;
  value?: string;
  touched?: boolean;
  description?: string;
  placeholder?: string;
  inputType?: "text" | "email" | "password";
  required?: boolean;
  error?: string;
  onChange?: (e: ChangeEvent<HTMLInputElement>) => void;
  onBlur?: (e: FocusEvent<HTMLInputElement>) => void;
}

const InputGroup = ({
  id,
  name = "",
  label,
  value,
  touched,
  description,
  placeholder,
  inputType = "text",
  required = false,
  error = "",
  onChange,
  onBlur,
}: InputGroupProps) => {
  const helpId = id + "-help";

  const inputClassNames = classnames(
    "focus:ring-indigo-500",
    "focus:border-indigo-500",
    "w-full",
    "rounded-md",
    "text-base",
    "shadow-sm",
    { ["border-red-700"]: error.length > 0 }
  );

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
        <p className="text-sm font-light text-gray-500">{description}</p>
      )}
      <div className="mt-1 rounded-md shadow-sm">
        <input
          type={inputType}
          name={name}
          value={value}
          className={inputClassNames}
          id={label && id}
          placeholder={placeholder}
          onChange={onChange}
          onBlur={onBlur}
          aria-describedby={error && helpId}
        />
      </div>
      {error && touched && (
        <span className="text-xs text-red-700" id={helpId}>
          {error}
        </span>
      )}
    </div>
  );
};

export default InputGroup;
