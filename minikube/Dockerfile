FROM golang:1.19 as builder

# Create and change to the app directory.
WORKDIR /app

# Copy go.mod and go.sum, then retrieve application dependencies.
# This allows the container build to reuse cached dependencies.
COPY go.* ./
RUN go mod download

# Copy local code to the container image.
COPY . ./

# Build the binary.
RUN CGO_ENABLED=0 GOOS=linux go build -mod=readonly -v -o main

# Use the official Alpine image for a lean production container.
# https://docs.docker.com/develop/develop-images/multistage-build/#use-multi-stage-builds
FROM alpine:latest
RUN apk add --no-cache ca-certificates

# Copy the binary to the production image from the builder stage.
COPY --from=builder /app/main /main

# Defaults
ARG DICE_GRPC_TARGET="server-service:3001"

# Env overrides
ENV DICE_GRPC_TARGET=${DICE_GRPC_TARGET}

# Run the web service on container startup.
CMD ["/main"]