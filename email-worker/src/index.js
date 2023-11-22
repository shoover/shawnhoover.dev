import { App } from "octokit";

//
// Support functions
//

async function streamToText(stream) {
  const reader = stream.getReader();
  const decoder = new TextDecoder('utf-8');
  let text = '';

  while (true) {
    const { done, value } = await reader.read();

    if (done) {
      return text;
    }

    text += decoder.decode(value);
  }
}

function headersToString(headers) {
  let result = '';
  for (const [name, value] of headers.entries()) {
    result += `${name}: ${value}\n`;
  }
  return result;
}

function senderAllowed(allowList, from) {
	return allowList.indexOf(from) >= 0;
}

function forwardAllowed(allowForwarderPattern, allowForwardedFor, from, forwardedFor) {
	return from.match(allowForwarderPattern) && forwardedFor === allowForwardedFor;
}

function assertEnv(env, name) {
	if (!env.hasOwnProperty(name)) {
		throw new Error(`env.${name} is required`);
	}

	return env[name];
}

async function authenticatedGitHubRestApi(owner, repo, appId, privateKey) {
		const app = new App({
			appId,
			privateKey,
		});

		console.log("Getting installation ID");

		const {
			data: { id },
		} = await app.octokit.request('GET /repos/{owner}/{repo}/installation', {
			owner,
			repo,
			headers: {
				'X-GitHub-Api-Version': '2022-11-28'
			}
		})

		console.log(`Installation: ${id}`);

		return await app.getInstallationOctokit(id);
}

//
// Email handler
//

export default {
	async email(message, env, ctx) {
		console.log(`Received message from ${message.from}\n${headersToString(message.headers)}`);

		const allowList = assertEnv(env, 'ALLOWED_SENDERS').split(",");
		const allowForwardedFor = assertEnv(env, 'ALLOWED_FORWARDED_FOR');
		const allowForwarderPattern = new RegExp(assertEnv(env, 'ALLOWED_FORWARDER_PATTERN'));

		const OWNER = assertEnv(env, 'OWNER');
		const REPO = assertEnv(env, 'REPO');
		const GITHUB_APP_ID = assertEnv(env, 'GITHUB_APP_ID');
		const GITHUB_PRIVATE_KEY = assertEnv(env, 'GITHUB_PRIVATE_KEY');

		// Initialize GitHub API access as an Email Worker Trigger app installation.
		const octokit = await authenticatedGitHubRestApi(OWNER, REPO, GITHUB_APP_ID, GITHUB_PRIVATE_KEY);

		// Log forwarding permission requests from Gmail.
		if (message.from === "forwarding-noreply@google.com") {
			console.log(await streamToText(message.raw));
			return;
		}

		const forwardedFor = message.headers.get("x-forwarded-for");

    if (!senderAllowed(allowList, message.from)) {
			console.log("Sender not allowed. Checking forward.");
			if (!forwardAllowed(allowForwarderPattern, allowForwardedFor, message.from, forwardedFor)) {
				console.log(`Rejecting:
  from: ${message.from}
  x-forwarded-for: ${forwardedFor}`);
				message.setReject("Not allowed");
				return;
			}

			console.log(`Forward allowed. Posting to GitHub.`);
		}

		console.log(`Posting dispatch event to GitHub ${OWNER}/${REPO}`);

		const response = await octokit.rest.repos.createDispatchEvent({
			owner: OWNER,
			repo: REPO,
			event_type: "email_received",
			client_payload: {},
		});

		console.log(`GitHub response: ${JSON.stringify(response, null, 2)}`);
	}
};
