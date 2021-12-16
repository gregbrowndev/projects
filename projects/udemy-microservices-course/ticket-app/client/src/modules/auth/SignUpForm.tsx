import Button from "../../components/Button";
import InputGroup from "../../components/InputGroup";
import { Formik, Field, Form } from "formik";
import { FormikHelpers } from "formik/dist/types";
import { passThroughSymbol } from "next/dist/server/web/spec-compliant/fetch-event";
import { FieldAttributes, FieldProps } from "formik/dist/Field";

export interface SignUpFormValues {
  email: string;
  password: string;
}

export interface SignUpFormSubmitError {
  title: string;
  detail?: string;
  errors: FieldErrors;
}

export type FieldErrors = {
  [k in keyof SignUpFormValues]?: string;
};

export interface SignUpFormProps {
  initialValues: SignUpFormValues;
  onSubmit: (values: SignUpFormValues) => Promise<SignUpFormSubmitError | null>;
}

const SignUpForm = ({
  initialValues = { email: "", password: "" },
  onSubmit,
}: SignUpFormProps) => {
  const onSubmitWrapper = async (
    values: SignUpFormValues,
    actions: FormikHelpers<SignUpFormValues>
  ) => {
    console.log("[SignUpForm] onSubmit called with: ", values);
    onSubmit(values).then((result) => {
      console.log("[SignUpForm] submission result: ", result);
      if (result) {
        actions.setErrors(result.errors || {});
      }
    });
  };
  return (
    <Formik initialValues={initialValues} onSubmit={onSubmitWrapper}>
      <Form>
        <div className="shadow-lg sm:rounded-md sm:overflow-hidden">
          <div className="px-4 py-5 bg-white space-y-6 sm:p-6">
            {/*{errors}*/}
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
            <Button type="submit" label="Sign Up" />
          </div>
        </div>
      </Form>
    </Formik>
  );
};

export default SignUpForm;
