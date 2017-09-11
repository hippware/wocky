# vim: set noexpandtab ts=2 sw=2:
.PHONY: help unittest inttest release build push deploy migrate shipit restart pods top exec console shell describe logs follow

VERSION ?= $(shell elixir ./version.exs)
IMAGE_REPO ?= 773488857071.dkr.ecr.us-west-2.amazonaws.com
IMAGE_NAME ?= hippware/wocky
IMAGE_TAG ?= $(shell git rev-parse HEAD)
WOCKY_ENV ?= testing
KUBE_NS := wocky-$(WOCKY_ENV)

help:
	@echo "Repo:    $(IMAGE_REPO)/$(IMAGE_NAME)"
	@echo "Tag:     $(IMAGE_TAG)"
	@echo "Version: $(VERSION)"
	@echo ""
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

unittest: ## Run the unit tests locally
	mix do lint, ecto.wait, ecto.reset, test, espec

inttest: ## Run the integration tests locally
	mix do ecto.wait, ecto.reset, epmd
	mix ct

release: ## Build the release tarball
	MIX_ENV=prod mix release --warnings-as-errors
	cp _build/prod/rel/wocky/releases/$(VERSION)/wocky.tar.gz /artifacts

build: ## Build the release Docker image
	rm -f ${PWD}/tmp/artifacts/wocky.tar.gz
	docker build . -t $(IMAGE_NAME):build -f Dockerfile.build
	docker run -it --rm \
		-v ${PWD}/tmp/artifacts:/artifacts \
		$(IMAGE_NAME):build make release
	docker build . -f Dockerfile.release \
		-t $(IMAGE_REPO)/$(IMAGE_NAME):$(IMAGE_TAG) \
		-t $(IMAGE_REPO)/$(IMAGE_NAME):latest

push: ## Push the Docker image to ECR
	docker push $(IMAGE_REPO)/$(IMAGE_NAME):$(IMAGE_TAG)
	docker push $(IMAGE_REPO)/$(IMAGE_NAME):latest

deploy: ## Deploy the image to the cluster
	kubectl set image deployment/wocky -n $(KUBE_NS) \
		wocky=$(IMAGE_REPO)/$(IMAGE_NAME):$(IMAGE_TAG)

migrate: ## Run the database migrations in a k8s job
	kubectl create -f k8s/wocky-migration-job.yml -n $(KUBE_NS)

shipit: build push deploy ## Build, push and deploy the image

restart: ## Do a rolling restart of the running pods
	@kubectl patch deployment wocky -n $(KUBE_NS) \
		-p'{"spec":{"template":{"spec":{"containers":[{"name":"wocky","env":[{"name":"RESTART_","value":"$(date -uIseconds)"}]}]}}}}'

pods: ## Return a list of running pods
	@kubectl get pods -n $(KUBE_NS) -l 'app=wocky' -o jsonpath='{.items[].metadata.name}'

top: ## Show resource usage for app pods
	@kubectl top pod -n $(KUBE_NS) -l 'app=wocky'

define first-pod
$(shell kubectl get pods -n $(KUBE_NS) -l 'app=wocky' -o jsonpath='{.items[0].metadata.name}')
endef

define do-exec
kubectl exec -it -n $(KUBE_NS) $(POD) $(1)
endef

exec: POD ?= $(first-pod)
exec: ## Execute $CMD on a pod
	$(call do-exec,$(CMD))

console: POD ?= $(first-pod)
console: ## Start an Iex remote console on a pod
	@$(call do-exec,bin/wocky remote_console)

shell: POD ?= $(first-pod)
shell: ## Start a shell on a pod
	@$(call do-exec,/bin/sh)

describe: POD ?= $(first-pod)
describe: ## Describe the current release on a pod
	@$(call do-exec,bin/wocky describe)

logs: POD ?= $(first-pod)
logs: ## Show the logs for a pod
	@kubectl logs -n $(KUBE_NS) $(POD)

follow: POD ?= $(first-pod)
follow: ## Follow the logs for a pod
	@kubectl logs -n $(KUBE_NS) -f $(POD)
