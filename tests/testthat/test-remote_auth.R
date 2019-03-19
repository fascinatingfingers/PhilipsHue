
context('remote_auth')

test_that('helper creates URL to request access to bridge from client', {
    mockery::stub(remote_auth_request_url, 'uuid::UUIDgenerate', 'mock_uuid')
    expect_identical(
        remote_auth_request_url('client_id', 'app id', 'device_id', 'device name'),
        'https://api.meethue.com/oauth2/auth?clientid=client_id&appid=app%20id&deviceid=device_id&devicename=device%20name&state=mock_uuid&response_type=code'
    )
})
