import * as L from "./logger";
const axios = require("axios");

function APIResult(result: any) {
    L.debug("APIResult:", result.config.url, result);
    return result.data;
}

export type Params = {
    baseURL: string;
    debug: boolean;
    env: string;
    version: string;
    onLoading: () => void;
    onLoadingDone: () => void;
};

export default class API {
    params: Params;
    api: any;
    inFlightRequests: number;

    constructor(params: Params) {
        this.params = {
            baseURL: "https://api.pitchy.ninja/v1",
            debug: true,
            env: "dev",
            version: "local",
            onLoading: () => {
                L.info("API LOADING");
            },
            onLoadingDone: () => {
                L.info("API LOADING DONE");
            },
            ...params,
        };

        this.api = axios.create({
            baseURL: this.params.baseURL,
            timeout: 20000,
            withCredentials: true, // required for CORS requests
            headers: {
                Accept: "application/json",
                "Content-Type": "application/json",
            },
        });

        if (this.api === null) {
            throw new Error("can't create API");
        }

        L.info("Using Pitchy backend: ", this.params.baseURL);
        this.inFlightRequests = 0;
    }

    onLoading() {
        if (this.inFlightRequests === 0) {
            this.params.onLoading();
        }
        this.inFlightRequests += 1;
    }

    onLoadingDone() {
        this.inFlightRequests -= 1;
        if (this.inFlightRequests === 0) {
            this.params.onLoadingDone();
        }
    }

    bolster(entity: any) {
        // eslint-disable-next-line
        entity["_env"] = this.params.env;
        // eslint-disable-next-line
        entity["_version"] = this.params.version;
    }

    post(url: string, params: any) {
        this.bolster(params);
        L.info("API Request: POST", url, params);
        this.onLoading();
        return this.api
            .post(url, JSON.stringify(params))
            .then((data: any) => {
                this.onLoadingDone();
                return APIResult(data);
            })
            .catch((e: Error) => {
                this.onLoadingDone();
                throw e;
            });
    }

    get(url: string) {
        L.info("API Request: GET", url);
        this.onLoading();
        return this.api
            .get(url)
            .then((data: any) => {
                this.onLoadingDone();
                return APIResult(data);
            })
            .catch((e: Error) => {
                this.onLoadingDone();
                throw e;
            });
    }

    ping() {
        return this.get("/ping");
    }

    authVerify(code: string) {
        return this.post("/auth/verify", { code });
    }

    authLogout() {
        return this.get("/auth/logout");
    }

    authWhoAmI() {
        return this.get("/auth/whoami");
    }

    addFollower(email: string) {
        return this.post("/followers", { email });
    }
}
