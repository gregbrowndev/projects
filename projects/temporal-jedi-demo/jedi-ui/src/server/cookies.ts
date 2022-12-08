import { OptionsType } from 'cookies-next/src/types';
import { getCookie, setCookie } from 'cookies-next';

const WORKFLOW_COOKIE_NAME = 'workflowId';

export function setWorkflowId(workflowId: string, options?: OptionsType): void {
  // Save workflowID as cookie so UI can track workflow
  console.log(`Setting workflow ID cookie: ${workflowId}`);
  setCookie(WORKFLOW_COOKIE_NAME, workflowId, options);
}

export function getWorkflowId(options?: OptionsType): string | undefined {
  const workflowId = getCookie(WORKFLOW_COOKIE_NAME, options);
  if (typeof workflowId == 'string') {
    console.log(`Found workflow ID: ${workflowId}`);
    return workflowId;
  }
  return undefined;
}

export function deleteWorkflowId(options?: OptionsType): void {
  console.log(`Deleting workflow ID cookie`);
  // deleteCookie(WORKFLOW_COOKIE_NAME, options);
  // deleting cookie doesn't persist in HTTPie session, set it to null instead
  setCookie(WORKFLOW_COOKIE_NAME, null, options);
}
