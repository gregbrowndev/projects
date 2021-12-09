import axios, { Method } from "axios";
import { ReactElement, useState } from "react";
import Alert from "../components/Alert";
import { Error } from "../common/models/error";

export interface UseRequestProps {
  url: string;
  method: Method;
  data: any;
  onSuccess?: (data: any) => void;
}

const useRequest = ({ url, method, data, onSuccess }: UseRequestProps) => {
  const [errors, setErrors] = useState<ReactElement | undefined>(undefined);

  const doRequest = async () => {
    setErrors(undefined);

    try {
      const response = await axios.request({ method, url, data });

      if (onSuccess) {
        onSuccess(response.data);
      }

      return response.data;
    } catch (err: Error | any) {
      let errors: Error[] = [{ message: "Something went wrong" }];

      if (err.response?.data?.errors.length > 0) {
        errors = err.response.data.errors;
      }

      const renderErrors = (
        <div className="p-2">
          <Alert variant="danger" title="Oops...">
            <ul className="text-sm font-light text-gray-500">
              {errors.map((error) => (
                <li key={error.message}>{error.message}</li>
              ))}
            </ul>
          </Alert>
        </div>
      );

      setErrors(renderErrors);
    }
  };

  return { doRequest, errors };
};

export default useRequest;
