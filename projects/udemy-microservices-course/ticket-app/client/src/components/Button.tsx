type Variant = "primary" | "secondary" | "tertiary";
type Size = "small" | "medium" | "large";

interface ButtonProps {
  /**
   * Is this the principal call to action on the page?
   */
  variant?: Variant;
  /**
   * How large should the button be?
   */
  size?: Size;
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
  variant = "primary",
  size = "medium",
  label,
  ...props
}: ButtonProps) => {
  const variantMap = new Map<Variant, string>([
    ["primary", "bg-indigo-600 hover:bg-indigo-800 text-white"],
    ["secondary", "bg-indigo-200 hover:bg-indigo-400 text-indigo-800"],
    ["tertiary", "bg-white hover:bg-indigo-400 text-indigo-800"],
  ]);

  const sizeMap = new Map<Size, string>([
    ["small", "py-1 px-2"],
    ["medium", "py-2 px-4"],
    ["large", "py-4 px-8"],
  ]);

  return (
    <button
      className={
        variantMap.get(variant) + " " + sizeMap.get(size) + " font-bold rounded"
      }
      onClick={props.onClick}
    >
      {label}
    </button>
  );
};

export default Button;
