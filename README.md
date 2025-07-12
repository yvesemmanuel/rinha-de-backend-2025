# Payment Processor

A Haskell-based payment processing API built with Servant for the Rinha de Backend 2025 challenge. This service acts as an intermediary between payment requests and external payment processors, implementing a fallback strategy for high availability.

## Features

- **RESTful API** with three main endpoints
- **Structured Logging** with correlation IDs and rich context
- **Modular Architecture** with clear separation of concerns
- **Abstract Storage Layer** (currently in-memory, easily extensible)
- **Payment Processor Abstraction** with mock implementation
- **Fallback Strategy** for payment processing resilience

## API Endpoints

### POST /payments
Process a payment request.

**Request:**
```json
{
  "correlationId": "550e8400-e29b-41d4-a716-446655440000",
  "amount": 100.50
}
```

**Response:** HTTP 2XX (any 200-level status code)

### GET /payments-summary
Get a summary of processed payments with optional date filtering.

**Query Parameters:**
- `from` (optional): ISO timestamp in UTC format
- `to` (optional): ISO timestamp in UTC format

**Response:**
```json
{
  "default": {
    "totalRequests": 150,
    "totalAmount": 15000.75
  },
  "fallback": {
    "totalRequests": 25,
    "totalAmount": 2500.25
  }
}
```

### GET /health
Health check endpoint.

**Response:** Plain text status message

## Architecture

The application is organized into the following modules:

### Core Modules

- **`Types.hs`** - Core data types and JSON serialization
- **`API.hs`** - Servant API type definitions
- **`Handlers.hs`** - HTTP request handlers
- **`Main.hs`** - Application entry point

### Business Logic

- **`PaymentService.hs`** - Payment processing and mock implementation
- **`Storage.hs`** - Storage abstraction with in-memory implementation

### Infrastructure

- **`Logger.hs`** - Structured logging with multiple formats and levels

## Project Structure

```
├── src/
│   ├── Main.hs           # Application entry point
│   ├── API.hs            # Servant API definitions
│   ├── Handlers.hs       # HTTP handlers
│   ├── Types.hs          # Core data types
│   ├── PaymentService.hs # Payment processing logic
│   ├── Storage.hs        # Data storage abstraction
│   └── Logger.hs         # Logging infrastructure
├── package.yaml          # Package configuration
├── stack.yaml            # Stack configuration
└── README.md             # This file
```

## Getting Started

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/) (Haskell build tool)
- GHC 9.8.4 (managed by Stack)

### Installation

1. **Clone the repository:**
   ```bash
   git clone <repository-url>
   cd payment-processor
   ```

2. **Build the project:**
   ```bash
   make build
   ```

3. **Run the application:**
   ```bash
   make run
   ```

The server will start on port 8080.

### Development

For development with automatic recompilation:

```bash
make build-dev
```

To run tests (when implemented):

```bash
make test
```

## Usage Examples

### Process a Payment

```bash
curl -X POST http://localhost:8080/payments \
  -H "Content-Type: application/json" \
  -d '{
    "correlationId": "550e8400-e29b-41d4-a716-446655440000",
    "amount": 100.50
  }'
```

### Get Payments Summary

```bash
# All payments
curl http://localhost:8080/payments-summary

# With date range
curl "http://localhost:8080/payments-summary?from=2025-01-01T00:00:00Z&to=2025-12-31T23:59:59Z"
```

### Health Check

```bash
curl http://localhost:8080/health
```

## Logging

The application features comprehensive structured logging with:

- **Multiple log levels**: DEBUG, INFO, WARN, ERROR, FATAL
- **Rich context**: Correlation IDs, component names, operations, metadata
- **Multiple formats**: Plain text and JSON
- **Configurable output**: stdout, stderr, or custom handles

### Log Output Example

```
[2025-07-11 10:30:15 UTC] INFO  payment_service [550e8400-e29b-41d4-a716-446655440000] (process_payment): Starting payment processing {amount=100.50}
[2025-07-11 10:30:15 UTC] INFO  storage [550e8400-e29b-41d4-a716-446655440000] (store_payment): Payment record stored successfully
```

## Configuration

### Log Levels

- **Production**: INFO and above
- **Development**: DEBUG and above
- **Error tracking**: ERROR and FATAL to stderr

### Storage

Currently uses in-memory storage. The storage layer is abstracted through the `PaymentStorage` typeclass, making it easy to implement:

- PostgreSQL storage
- Redis storage
- File-based storage
- Any other storage backend

### Payment Processing

Uses a mock payment processor that simulates:
- 80% success rate with default processor
- 20% fallback to secondary processor
- Realistic processing delays and failures

## Dependencies

- **servant** - Web framework
- **servant-server** - Servant server implementation
- **wai** / **warp** - Web application interface and server
- **aeson** - JSON parsing and encoding
- **uuid** - UUID generation and handling
- **time** - Date and time handling
- **text** - Efficient text processing
- **hashable** - Hash functions
- **containers** - Data structures


## Future Enhancements

- [ ] Database integration (PostgreSQL)
- [ ] Real payment processor clients
- [ ] Configuration management
- [ ] Comprehensive test suite
- [ ] Performance monitoring
- [ ] Circuit breaker pattern
- [ ] Rate limiting
- [ ] API documentation with Swagger
- [ ] Docker containerization
- [ ] Health check improvements
