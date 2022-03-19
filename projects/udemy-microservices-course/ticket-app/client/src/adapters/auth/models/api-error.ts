export interface ApiError {
  title: string;
  detail?: string;
  statusCode: number;
  invalidParams?: InvalidParam[];
}

export interface InvalidParam {
  name: string;
  reason: string;
}
