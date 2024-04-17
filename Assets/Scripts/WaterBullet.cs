using System;
using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;

public class WaterBullet : MonoBehaviour//脚本用于单个龙王的水弹
{
    public float lifespan = 5f; // 子弹生存时间
    public AnimationCurve speedCurve; // 控制速度变化的曲线
    public float Speed;

    private float startTime;
    private float timer;
    private Transform cameraTransform;
    public List<ParticleSystem> ballParticles = new List<ParticleSystem>();
    public GameObject WaterExplodeEffect;
    private float waterExplodeTime;
    private bool hasExploded = false;

    private void Start()
    {
        startTime = Time.time;
        cameraTransform = Camera.main.transform;
        timer = 0f;
        var main = WaterExplodeEffect.GetComponent<ParticleSystem>().main;
        waterExplodeTime = main.startLifetime.constant;
    }

    private void FixedUpdate()
    {
        //Ball Movement
        float timeElapsed = Time.time - startTime;
        float k = speedCurve.Evaluate(timeElapsed);
        transform.position += (cameraTransform.position - transform.position).normalized * k *Speed* Time.fixedDeltaTime;

        //Particle Effect Control
        timer += Time.fixedDeltaTime;
        if (timer >= lifespan - waterExplodeTime&&!hasExploded)
        {
                Debug.Log(waterExplodeTime);
                hasExploded = true;
                WaterExplode();
        }
    }

    public void WaterExplode()
    {
        WaterExplodeEffect.SetActive(true);
        foreach (var particle in ballParticles)
        {
            particle.gameObject.SetActive(false);
        }
        Destroy(this.gameObject,waterExplodeTime);
    }
    private void OnTriggerEnter(Collider other)
    {
        if (other.transform.parent.CompareTag("MainCamera"))
        {
            WaterExplode();
        }
    }
}