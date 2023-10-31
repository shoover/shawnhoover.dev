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

export default {
	async email(message, env, ctx) {
		const allowList = assertEnv(env, 'ALLOWED_SENDERS').split(",");
		const allowForwardedFor = assertEnv(env, 'ALLOWED_FORWARDED_FOR');
		const allowForwarderPattern = new RegExp(assertEnv(env, 'ALLOWED_FORWARDER_PATTERN'));

		const owner_repo = assertEnv(env, 'OWNER_REPO');
		const token = assertEnv(env, 'GITHUB_TOKEN');

		console.log(`Received message from ${message.from}\n${headersToString(message.headers)}`);

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

		const url = `https://api.github.com/repos/${owner_repo}/dispatches`;

		console.log(`Posting to GitHub: ${url}`);

		const response = await fetch(url, {
      method: 'POST',
			headers: {
				"Authorization": `Bearer ${token}`,
				"X-GitHub-Api-Version": "2022-11-28",
				"Content-Type": "application/json;charset=UTF-8",
				"Accept": "application/vnd.github+json",
				"User-Agent": `${owner_repo} email-worker`,
			},
			body: JSON.stringify({
				"event_type": "email_received",
				"client_payload": {}
			}),
    });

		const response_body = await response.text();

		console.log(`GitHub response: ${response.status} ${response_body}`);
	}
};
