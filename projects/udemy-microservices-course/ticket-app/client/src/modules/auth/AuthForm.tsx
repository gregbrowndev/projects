import Button from "../../components/Button";
import InputGroup from "../../components/InputGroup";
import { Formik, Field, Form, FieldProps, FormikHelpers } from "formik";

export interface AuthFormValues {
  email: string;
  password: string;
}

export interface AuthFormSubmitError {
  title: string;
  detail?: string;
  errors: FieldErrors;
}

export type FieldErrors = {
  [k in keyof AuthFormValues]?: string;
};

export interface AuthFormProps {
  type: "sign-in" | "sign-up";
  initialValues: AuthFormValues;
  onSubmit: (values: AuthFormValues) => Promise<AuthFormSubmitError | null>;
}

const AuthForm = ({
  type,
  initialValues = { email: "", password: "" },
  onSubmit,
}: AuthFormProps) => {
  const onSubmitWrapper = async (
    values: AuthFormValues,
    actions: FormikHelpers<AuthFormValues>
  ) => {
    console.log("[AuthForm] onSubmit called with: ", values);
    onSubmit(values).then((result) => {
      console.log("[AuthForm] submission result: ", result);
      if (result) {
        actions.setErrors(result.errors || {});
      }
    });
  };
  return (
    <Formik initialValues={initialValues} onSubmit={onSubmitWrapper}>
      {({ isSubmitting, status, errors }) => (
        <Form>
          <div className="shadow-lg sm:rounded-md sm:overflow-hidden">
            <div className="px-4 py-5 bg-white space-y-6 sm:p-6">
              <Field name="email">
                {({ field, form, meta }: FieldProps) => (
                  <InputGroup
                    id="email-control"
                    inputType="email"
                    label="Email Address"
                    required
                    placeholder="you@example.com"
                    value={field.value}
                    name={field.name}
                    onChange={field.onChange}
                    onBlur={field.onBlur}
                    error={meta.error}
                    touched={meta.touched}
                  />
                )}
              </Field>
              <Field name="password">
                {({ field, form, meta }: FieldProps) => (
                  <InputGroup
                    id="password-control"
                    inputType="password"
                    label="Password"
                    required
                    value={field.value}
                    name={field.name}
                    onChange={field.onChange}
                    onBlur={field.onBlur}
                    error={meta.error}
                    touched={meta.touched}
                  />
                )}
              </Field>
            </div>
            <div className="px-4 py-3 bg-gray-50 text-right sm:px-6">
              <Button
                type="submit"
                label={type === "sign-up" ? "Sign Up" : "Sign In"}
              />
            </div>
          </div>
        </Form>
      )}
    </Formik>
  );
};

export default AuthForm;
