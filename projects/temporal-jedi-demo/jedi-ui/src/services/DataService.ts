import { getCookie } from 'cookies-next';

export function getWorkflowIdCookie(): string | undefined {
  const cookieVal = getCookie('workflowId');
  if (typeof cookieVal == 'string') {
    console.log(`Found workflow ID: ${cookieVal}`);
    return cookieVal;
  }
}

export async function startWorkflow(): Promise<void> {
  await fetch('/api/startWorkflow', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
  });
}

export async function deleteWorkflow(): Promise<void> {
  await fetch('/api/deleteWorkflow', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
  });
}
