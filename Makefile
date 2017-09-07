.PHONY: help unittest inttest release build push deploy migrate shipit

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
	kubectl create -f k8s/wocky-migration-job.yaml -n $(KUBE_NS)

shipit: build push deploy ## Build, push and deploy the image
