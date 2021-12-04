interface ButtonProps {
  /**
   * Is this the principal call to action on the page?
   */
  type?: "primary" | "secondary" | "white";
  /**
   * How large should the button be?
   */
  size?: "small" | "medium" | "large";
  /**
   * Button contents
   */
  label: string;
  /**
   * Optional click handler
   */
  onClick?: () => void;
}

const Button = ({
  type = "primary",
  size = "medium",
  label,
  ...props
}: ButtonProps) => {
  const typeMap = new Map<string, string>([
    ["primary", "bg-indigo-600 hover:bg-indigo:900 text-white"],
    ["secondary", "bg-indigo-200 hover:bg-indigo:400 text-indigo-800"],
    ["white", "bg-white hover:bg-indigo:400 text-indigo-800"],
  ]);

  const sizeMap = new Map<string, string>([
    ["small", "py-1 px-2"],
    ["medium", "py-2 px-4"],
    ["large", "py-4 px-8"],
  ]);

  return (
    <button
      className={
        typeMap.get(type) + " " + sizeMap.get(size) + " font-bold rounded"
      }
      onClick={props.onClick}
    >
      {label}
    </button>
  );
};

export default Button;
