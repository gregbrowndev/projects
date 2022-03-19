import axios, { Method } from "axios";
import { ReactElement, useState } from "react";
import Alert from "../components/Alert";
import { ApiError } from "../adapters/auth/models/api-error";

export interface UseRequestConfig<T = any, D = any> {
  url: string;
  method: Method;
  data: D;
  onSuccess?: (obj: T) => void;
}

export interface UseRequestOutput<T> {
  doRequest: () => Promise<T>;
  errors: ReactElement | undefined;
}

export interface UseRequestFunc {
  <T = any, D = any>(config: UseRequestConfig<T, D>): UseRequestOutput<T>;
}

const useRequest: UseRequestFunc = ({ url, method, data, onSuccess }) => {
  const [errors, setErrors] = useState<ReactElement | undefined>(undefined);

  const doRequest = async () => {
    setErrors(undefined);

    try {
      const response = await axios.request({ method, url, data });

      if (onSuccess) {
        onSuccess(response.data);
      }

      return response.data;
    } catch (err: ApiError | any) {
      let error: ApiError = { title: "Something went wrong", statusCode: 500 };

      if (err.response?.data) {
        error = err.response.data;
      }

      const renderErrors = (
        <div className="p-2">
          <Alert variant="danger" title="Oops...">
            <h3 className="text-base font-light text-gray-500">
              {error.title}
            </h3>
          </Alert>
        </div>
      );

      setErrors(renderErrors);
    }
  };

  return { doRequest, errors };
};

export default useRequest;
