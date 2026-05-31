# Garnix

Garnix is a CI service for nixified, flake-based github repos.

## Running Garnix locally in VMs

You can spin up a couple of qemu VMs that provide a full Garnix deployment with:

```bash
nix run -L .#examples_spinUpVms
```

This will use [`nixos-compose`](https://github.com/garnix-io/nixos-compose).
If you run:

```bash
nixos-compose tap
nixos-compose status
```

You should then be able to point your browser to the ip address of the `exampleGarnixServer` to see the hosted ci.

And there's an admin page on `/garnix-admin` that is useful for some development tasks.

### Setting up a GitHub app

You _will_ need a github app for Garnix to work, both for production and for testing.
On the `/garnix-admin` page you can create one by pressing the 'Submit to GitHub' button.
That will give you a bunch of credentials that you'll have to put into the `/secrets/dev.yaml` file by running

```bash
sops edit secrets/dev.yaml
```

Then you have to enable your new GitHub app on a repo that you want to build through the GitHub ui.
Finally, you can submit a test build, with something like this:

```bash
curl -v \
  -XPOST \
  http://$(nixos-compose ip exampleGarnixServer)/api/build/submit \
  -H 'Content-Type: application/json' \
  -d '{ "owner": "garnix-io", "repo": "comment", "testCommit": "8b2b57d91dd1f4d094bb944a0a0ef65319a5663f" }'
```

And then you can see the build under `/repo/garnix-io/comment`, for example.

### Developing the frontend

You can run the frontend in development mode against a backend in a VM like this:

```bash
nixos-compose up -v
cd frontend
npm run dev
```

Then point your browser to [localhost:3000](http://localhost:3000).


# Acknowledgments

We erased git history when open sourcing, so we'll be explicit here about our
debt to everyone who contributed before the project became open source:

- Alex David
- Evie Ciobanu
- Greg Pfeil
- Jean-François Roche
- Julian Kirsten Arni
- Ramses de Norre
- Sönke Hahn

Thanks very very much!
