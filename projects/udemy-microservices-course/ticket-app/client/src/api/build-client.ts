import axios, { AxiosRequestHeaders } from "axios";
import { IncomingMessage } from "http";

const isString = (arg: any): arg is string =>
  arg !== undefined && typeof arg === "string";

const buildClient = (req: IncomingMessage) => {
  if (typeof window === "undefined") {
    const headers: AxiosRequestHeaders = Object.fromEntries<string>(
      Object.entries(req.headers).filter((item): item is [string, string] =>
        isString(item[1])
      )
    );

    return axios.create({
      baseURL:
        "http://ingress-nginx-controller.ingress-nginx.svc.cluster.local",
      headers: headers,
    });
  } else {
    return axios.create({
      baseURL: "/",
    });
  }
};

export default buildClient;
