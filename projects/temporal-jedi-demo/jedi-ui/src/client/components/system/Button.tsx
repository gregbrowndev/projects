import { spans } from 'next/dist/build/webpack/plugins/profiling-plugin';

type Variant = 'primary' | 'secondary' | 'tertiary';
type Size = 'small' | 'medium' | 'large';

export interface ButtonProps {
  /**
   * Is this the principal call to action on the page?
   */
  variant?: Variant;
  /**
   * HTML type of the component
   */
  type: 'button' | 'submit' | 'reset';
  /**
   * How large should the button be?
   */
  size?: Size;
  /**
   * Button contents
   */
  label: string;
  /**
   * Disabled
   */
  disabled?: boolean;
  /**
   * Loading - show this button in a loading state
   */
  loading?: boolean;
  /**
   * Optional click handler
   */
  onClick?: () => void;
}

const Button: React.FC<ButtonProps> = ({
  variant = 'primary',
  type = 'button',
  size = 'medium',
  disabled = false,
  loading = false,
  label,
  ...props
}) => {
  const variantMap = new Map<Variant, string>([
    [
      'primary',
      'bg-indigo-600 hover:bg-indigo-800 text-white disabled:bg-gray-300',
    ],
    [
      'secondary',
      'bg-indigo-200 hover:bg-indigo-400 text-indigo-800 disabled:bg-gray-300',
    ],
    [
      'tertiary',
      'bg-white hover:bg-indigo-400 text-indigo-800 disabled:bg-gray-300',
    ],
  ]);

  const sizeMap = new Map<Size, string>([
    ['small', 'py-1 px-2 text-sm'],
    ['medium', 'py-1 px-2 text-sm md:py-2 md:px-4 md:text-base'],
    ['large', 'py-2 px-4 text-base md:py-2 md:px-6 md:text-lg'],
  ]);

  const Spinner = (
    <svg
      className="mr-3 h-5 w-5 animate-spin rounded-full border-2 border-transparent border-opacity-50 bg-transparent"
      style={{ borderRight: 'white', borderTopColor: 'white' }}
      viewBox="0 0 24 24"
    ></svg>
  );

  return (
    <button
      type={type}
      className={
        variantMap.get(variant) +
        ' ' +
        sizeMap.get(size) +
        ' inline-flex rounded font-bold'
      }
      onClick={props.onClick}
      disabled={disabled}
    >
      {loading && Spinner}
      {label}
    </button>
  );
};

export default Button;
