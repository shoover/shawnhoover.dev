import { App } from "octokit";

function assertEnv(env, name) {
	if (!env.hasOwnProperty(name)) {
		console.log(`env.${name} is required`);
		throw new Error(`env.${name} is required`);
	}

	return env[name];
}

const app = new App({
  appId: assertEnv(process.env, 'APP_ID'),
  privateKey: assertEnv(process.env, 'PRIVATE_KEY'),
});

console.log("Getting installation ID");

const {
	data: { id },
} = await app.octokit.request('GET /repos/{owner}/{repo}/installation', {
  owner: 'shoover',
  repo: 'shawnhoover.dev',
  headers: {
    'X-GitHub-Api-Version': '2022-11-28'
  }
})

console.log(`Installation: ${id}`);

const octokit = await app.getInstallationOctokit(id);


const response = await octokit.rest.repos.createDispatchEvent({
  owner: 'shoover',
  repo: 'shawnhoover.dev',
  event_type: "email_received",
  client_payload: {},
});

console.log(`GitHub response: ${JSON.stringify(response, null, 2)}`);
