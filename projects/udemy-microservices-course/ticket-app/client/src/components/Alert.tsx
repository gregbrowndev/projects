import {
  HiCheckCircle,
  HiExclamation,
  HiExclamationCircle,
  HiInformationCircle,
} from "react-icons/hi";
import { match } from "ts-pattern";
import { ReactNode } from "react";

export type Variant = "success" | "warning" | "danger" | "info";

export interface AlertProps {
  /**
   * Optional title to display above the description text
   */
  title?: string;
  /**
   * Description text to display under the component title.
   */
  children: string | ReactNode;
  /**
   * Type of component to display
   */
  variant: Variant;
}

const Alert = ({ title, children, variant }: AlertProps) => {
  const variantMap = new Map<Variant, string>([
    ["success", "teal"],
    ["warning", "orange"],
    ["danger", "red"],
    ["info", "blue"],
  ]);

  const variantColour = variantMap.get(variant);
  const backgroundColour = `bg-${variantColour}-50`;
  const borderColour = `border-${variantColour}-600`;
  const iconColour = `text-${variantColour}-500`;
  const titleColour = `text-${variantColour}-900`;

  const iconClasses = "fill-current h-4 w-4 mr-2 " + iconColour;
  const icon = match(variant)
    .with("success", () => <HiCheckCircle className={iconClasses} />)
    .with("warning", () => <HiExclamation className={iconClasses} />)
    .with("danger", () => <HiExclamationCircle className={iconClasses} />)
    .with("info", () => <HiInformationCircle className={iconClasses} />)
    .exhaustive();

  const description =
    typeof children === "string" ? (
      <p className="text-sm font-light text-gray-500">{children}</p>
    ) : (
      children
    );

  return (
    <div
      className={
        backgroundColour +
        " py-2 px-2 shadow-lg rounded-md border border-gray-300"
      }
    >
      <div className={"border-l-4 pl-2 flex flex-row " + borderColour}>
        <div className="py-1 ">{icon}</div>
        <div>
          {title && (
            <h2 className={"font-bold text-xl " + titleColour}>{title}</h2>
          )}
          {description}
        </div>
      </div>
    </div>
  );
};

export default Alert;
