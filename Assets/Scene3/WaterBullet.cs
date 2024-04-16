using System;
using System.Collections;
using Unity.VisualScripting;
using UnityEngine;

public class WaterBullet : MonoBehaviour
{
    public float lifespan = 5f; // 子弹生存时间
    public AnimationCurve speedCurve; // 控制速度变化的曲线
    public float Speed;

    private float startTime;
    private Transform cameraTransform;

    private void Start()
    {
        startTime = Time.time;
        cameraTransform = Camera.main.transform;
        Destroy(this.gameObject,lifespan);
    }

    private void Update()
    {
        float timeElapsed = Time.time - startTime;
        float k = speedCurve.Evaluate(timeElapsed);
        transform.position += (cameraTransform.position - transform.position).normalized * k *Speed* Time.deltaTime;
    }

    private void OnTriggerEnter(Collider other)
    {
        if (other.transform.parent.CompareTag("MainCamera"))
        {
            Debug.Log("111");
            Destroy(this.gameObject);
        }
    }
}